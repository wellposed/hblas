{-# LANGUAGE BangPatterns , RankNTypes #-}


module Numerical.OpenBLAS.BLAS where
--(
--    dgemm
--    ,sgemm
--    ,cgemm
--    ,zgemm) 


import Numerical.OpenBLAS.UtilsFFI    
import Numerical.OpenBLAS.BLAS.FFI 
import Numerical.OpenBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

--
flopsThreshold = 10000
gemmComplexity a b c = a * b * c  -- this will be wrong by some constant factor, albeit a small one


-- this covers the ~6 cases for checking the dimensions for GEMM quite nicely
isBadGemm tra trb  ax ay bx by cx cy = isBadGemmHelper (cds tra (ax,ay)) (cds trb (bx,by) )  (cx,cy)
    where 
    cds = coordSwapper 
    isBadGemmHelper !(ax,ay) !(bx,by) !(cx,cy) =  (minimum [ax, ay, bx, by, cx ,cy] <= 0)  
        || not (  cy ==  ay && cx == bx && ax == by)

coordSwapper :: Transpose -> (a,a)-> (a,a)
coordSwapper NoTranspose (a,b) = (a,b)
coordSwapper ConjNoTranspose (a,b) = (a,b) 
coordSwapper Transpose (a,b) = (b,a)
coordSwapper ConjTranspose (a,b) = (b,a)


encodeNiceOrder :: SOrientation x  -> CBLAS_ORDERT
encodeNiceOrder sor = encodeNiceOrderHelper (DenseMatrix sor undefined undefined undefined undefined)
    where 
        --encodeNiceOrderHelper :: SOrientation x  -> CBLAS_ORDERT
        encodeNiceOrderHelper (DenseMatrix SRow _ _ _ _ )= encodeOrder  BLASRowMajor
        encodeNiceOrderHelper (DenseMatrix SColumn _ _ _ _ )= encodeOrder BLASColMajor
{-

weirdly i can't pattern match on this Singleton directly, 

encodeNiceOrder :: SOrientation x  -> CBLAS_ORDERT
encodeNiceOrder SRow = encodeOrder  BLASRowMajor
encodeNiceOrder SColumn = encodeOrder BLASColMajor

-}



encodeFFITranpose  x=  encodeTranpose $ encodeNiceTranpose x 

encodeNiceTranpose :: Transpose -> BLAS_Transpose
encodeNiceTranpose x = case x of 
        NoTranspose -> BlasNoTranspose
        Transpose -> BlasTranspose
        ConjTranspose -> BlasConjTranspose
        ConjNoTranspose -> BlasConjNoTranspose

--data BLAS_Tranpose = BlasNoTranspose | BlasTranpose | BlasConjTranspose | BlasConjNoTranpose 
--data Tranpose = NoTranpose | Tranpose | ConjTranpose | ConjNoTranpose


type GemmFun el orient s m = Transpose ->Transpose ->  el -> el  -> MutDenseMatrix s orient el
  ->   MutDenseMatrix s orient el  ->  MutDenseMatrix s orient el -> m ()


{-
A key design goal of this ffi is to provide *safe* throughput guarantees 
for a concurrent application built on top of these apis, while evading
any overheads for providing such safety. Accordingly, on inputs sizes

-}


---- |  Matrix mult for general dense matrices
--type GemmFunFFI scale el = CBLAS_ORDERT ->   CBLAS_TRANSPOSET -> CBLAS_TRANSPOSET->
        --CInt -> CInt -> CInt -> {- scal A * B -} scale  -> {- Matrix A-} Ptr el  -> CInt -> {- B -}  Ptr el -> CInt-> 
            --scale -> {- C -}  Ptr el -> CInt -> IO ()
--type GemmFun = MutDenseMatrix or el ->  MutDenseMatrix or el ->   MutDenseMatrix or el -> m ()


gemmAbstraction:: (SM.Storable el, PrimMonad m) =>  
    GemmFunFFI scale el -> GemmFunFFI scale el -> (el -> (scale -> m a)->m a) -> forall orient . GemmFun el orient (PrimState m) m 
gemmAbstraction gemmSafeFFI gemmUnsafeFFI constHandler = go 
  where 
    go  tra trb  alpha beta 
        a@(MutableDenseMatrix ornta ax ay astride abuff) 
        b@(MutableDenseMatrix orntb bx by bstride bbuff) 
        c@(MutableDenseMatrix orntc cx cy cstride cbuff) 
            |  isBadGemm tra trb  ax ay bx by cx cy = error $! "bad dimension args to GEMM: ax ay bx by cx cy: " ++ show [ax, ay, bx, by, cx ,cy]
            | otherwise  = do 
                ap<- unsafeWithPrim abuff
                bp <- unsafeWithPrim bbuff
                cp <- unsafeWithPrim cbuff 
                constHandler alpha $  \alphaPtr -> do 
                    constHandler beta $ \betaPtr -> do 
                        (ax,ay) <- return $ coordSwapper tra (ax,ay)
                        (bx,by) <- return $ coordSwapper trb (bx,by)
                        --- c doesn't get implicitly transposed
                        blasOrder <- return $ encodeNiceOrder ornta -- all three are the same orientation
                        rawTra <- return $  encodeFFITranpose tra 
                        rawTrb <- return $   encodeFFITranpose trb
                                 -- example of why i want to switch to singletones
                        unsafePrimToPrim $!  (if shouldCallFast cy cx ax then gemmUnsafeFFI  else gemmSafeFFI ) 
                            blasOrder rawTra rawTrb (fromIntegral cy) (fromIntegral cx) (fromIntegral ax) 
                                alphaPtr ap  (fromIntegral astride) bp (fromIntegral bstride) betaPtr  cp (fromIntegral cstride)
                        return () 

   
    shouldCallFast :: Int -> Int -> Int -> Bool                         
    shouldCallFast cy cx ax = flopsThreshold >= gemmComplexity cy cx ax

