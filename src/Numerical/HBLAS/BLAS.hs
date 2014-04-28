{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}


module Numerical.HBLAS.BLAS where
--(
--    dgemm
--    ,sgemm
--    ,cgemm
--    ,zgemm) 

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI    
import Numerical.HBLAS.BLAS.FFI 
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import Data.Complex 
import qualified Data.Vector.Storable.Mutable as SM
import Numerical.HBLAS.Constants
import Data.Int 

gemmComplexity :: Integral a => a -> a -> a -> Int64
gemmComplexity a b c = fromIntegral a * fromIntegral b *fromIntegral c  -- this will be wrong by some constant factor, albeit a small one

gemvComplexity :: Integral a => a -> a -> Int64
gemvComplexity a b = fromIntegral a * fromIntegral b 


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

-- / checks if the size of a matrices rows matches input vector size 
-- and the  column count matchesresult vector size
isBadGemv :: Transpose -> Int -> Int -> Int -> Int -> Bool  
isBadGemv tr ax ay bdim cdim = isBadGemvHelper (cds tr (ax,ay))
    where 
    cds = coordSwapper
    isBadGemvHelper (realX,realY)  =  
            minimum [realY,realX,bdim,cdim] <= 0 ||  not (realX == bdim && realY == cdim )


encodeNiceOrder :: SOrientation x  -> CBLAS_ORDERT
encodeNiceOrder SRow= encodeOrder  BLASRowMajor
encodeNiceOrder SColumn= encodeOrder BLASColMajor


encodeFFITranspose :: Transpose -> CBLAS_TRANSPOSET
encodeFFITranspose  x=  encodeTranspose $ encodeNiceTranspose x 

encodeNiceTranspose :: Transpose -> BLAS_Transpose
encodeNiceTranspose x = case x of 
        NoTranspose -> BlasNoTranspose
        Transpose -> BlasTranspose
        ConjTranspose -> BlasConjTranspose
        ConjNoTranspose -> BlasConjNoTranspose

encodeFFIMatrixHalf :: MatUpLo -> CBLAS_UPLOT
encodeFFIMatrixHalf x = encodeUPLO $ encodeNiceUPLO x

encodeNiceUPLO :: MatUpLo -> BLASUplo
encodeNiceUPLO x = case x of
                    MatUpper  -> BUpper
                    MatLower  -> BLower

encodeFFITriangleSort :: MatDiag -> CBLAS_DIAGT
encodeFFITriangleSort x = encodeDiag $ encodeNiceDIAG x

encodeNiceDIAG :: MatDiag -> BlasDiag
encodeNiceDIAG x = case x of
                    MatUnit    -> BlasUnit
                    MatNonUnit -> BlasNonUnit

--data BLAS_Transpose = BlasNoTranspose | BlasTranspose | BlasConjTranspose | BlasConjNoTranspose 
--data Transpose = NoTranspose | Transpose | ConjTranspose | ConjNoTranspose


type GemmFun el orient s m = Transpose ->Transpose ->  el -> el  -> MDenseMatrix s orient el
  ->   MDenseMatrix s orient el  ->  MDenseMatrix s orient el -> m ()

type GemvFun el orient s m = Transpose -> el -> el
  -> MDenseMatrix s orient el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m ()


type TrsvFun el orient s m =
      MatUpLo -> Transpose -> MatDiag
   -> MDenseMatrix s orient el -> MDenseVector s Direct el -> m ()  


{-
A key design goal of this ffi is to provide *safe* throughput guarantees 
for a concurrent application built on top of these apis, while evading
any overheads for providing such safety. Accordingly, on inputs sizes
where the estimated flops count will be more then 1-10 microseconds,
safe ffi calls are used. For inputs whose runtime is under that
unsafe ffi calls are used. 


-}


---- |  Matrix mult for general dense matrices
--type GemmFunFFI scale el = CBLAS_ORDERT ->   CBLAS_TRANSPOSET -> CBLAS_TRANSPOSET->
        --CInt -> CInt -> CInt -> {- scal A * B -} scale  -> {- Matrix A-} Ptr el  -> CInt -> {- B -}  Ptr el -> CInt-> 
            --scale -> {- C -}  Ptr el -> CInt -> IO ()
--type GemmFun = MutDenseMatrix or el ->  MutDenseMatrix or el ->   MutDenseMatrix or el -> m ()

{-# NOINLINE gemmAbstraction #-}
gemmAbstraction:: (SM.Storable el, PrimMonad m) =>  String -> 
    GemmFunFFI scale el -> GemmFunFFI scale el -> (el -> (scale -> m ())->m ()) -> forall orient . GemmFun el orient (PrimState m) m 
gemmAbstraction gemmName gemmSafeFFI gemmUnsafeFFI constHandler = go 
  where 
    shouldCallFast :: Int -> Int -> Int -> Bool                         
    shouldCallFast cy cx ax = flopsThreshold >= gemmComplexity cy cx ax

    go  tra trb  alpha beta 
        (MutableDenseMatrix ornta ax ay astride abuff) 
        (MutableDenseMatrix _ bx by bstride bbuff) 
        (MutableDenseMatrix _ cx cy cstride cbuff) 
            |  isBadGemm tra trb  ax ay bx by cx cy = error $! "bad dimension args to GEMM: ax ay bx by cx cy: " ++ show [ax, ay, bx, by, cx ,cy]
            | SM.overlaps abuff cbuff || SM.overlaps bbuff cbuff = 
                    error $ "the read and write inputs for: " ++ gemmName ++ " overlap. This is a programmer error. Please fix." 
            | otherwise  = 
                {-  FIXME : Add Sharing check that also errors out for now-}
                unsafeWithPrim abuff $ \ap -> 
                unsafeWithPrim bbuff $ \bp ->  
                unsafeWithPrim cbuff $ \cp  -> 
                constHandler alpha $  \alphaPtr ->   
                constHandler beta $ \betaPtr ->  
                    do  (ax,ay) <- return $ coordSwapper tra (ax,ay)
                        --- dont need to swap b, info is in a and c
                        --- c doesn't get implicitly transposed
                        blasOrder <- return $ encodeNiceOrder ornta -- all three are the same orientation
                        rawTra <- return $  encodeFFITranspose tra 
                        rawTrb <- return $   encodeFFITranspose trb
                                 -- example of why i want to switch to singletones
                        unsafePrimToPrim $!  (if shouldCallFast cy cx ax then gemmUnsafeFFI  else gemmSafeFFI ) 
                            blasOrder rawTra rawTrb (fromIntegral cy) (fromIntegral cx) (fromIntegral ax) 
                                alphaPtr ap  (fromIntegral astride) bp (fromIntegral bstride) betaPtr  cp (fromIntegral cstride)


{-pureGemm :: PrimMonad m=>
(Transpose ->Transpose ->  el -> el  -> MutDenseMatrix (PrimState m) orient el
      ->   MutDenseMatrix (PrimState m) orient el  ->  
                                    MutDenseMatrix (PrimState m) orient el -> m ())->
  Transpose ->Transpose ->  el -> el  -> DenseMatrix  orient el
  ->   DenseMatrix  orient el  ->  DenseMatrix  orient el  -}

sgemm :: PrimMonad m=> 
     Transpose ->Transpose ->  Float -> Float  -> MDenseMatrix (PrimState m) orient Float
  ->   MDenseMatrix (PrimState m) orient Float  ->  MDenseMatrix (PrimState m) orient Float -> m ()
sgemm =  gemmAbstraction "sgemm"  cblas_sgemm_safe cblas_sgemm_unsafe (\x f -> f x )                                 
                        

dgemm :: PrimMonad m=> 
     Transpose ->Transpose ->  Double -> Double -> MDenseMatrix (PrimState m) orient Double
  ->   MDenseMatrix (PrimState m) orient Double   ->  MDenseMatrix (PrimState m) orient Double -> m ()
dgemm = gemmAbstraction "dgemm"  cblas_dgemm_safe cblas_dgemm_unsafe  (\x f -> f x )    
 

cgemm :: PrimMonad m=>  Transpose ->Transpose ->  (Complex Float) -> (Complex Float)  -> 
        MDenseMatrix (PrimState m) orient (Complex Float)  ->   
        MDenseMatrix (PrimState m) orient (Complex Float)  ->  
        MDenseMatrix (PrimState m) orient (Complex Float) -> m ()
cgemm = gemmAbstraction "cgemm" cblas_cgemm_safe cblas_cgemm_unsafe  withRStorable_                                

zgemm :: PrimMonad m=>  Transpose ->Transpose ->  (Complex Double) -> (Complex Double )  -> 
        MDenseMatrix (PrimState m) orient (Complex Double )  ->   
        MDenseMatrix (PrimState m) orient (Complex Double)  ->  
        MDenseMatrix (PrimState m) orient (Complex Double) -> m ()
zgemm = gemmAbstraction "zgemm"  cblas_zgemm_safe cblas_zgemm_unsafe withRStorable_  


{-# NOINLINE gemvAbstraction #-}
gemvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> GemvFunFFI scale el
                -> GemvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . GemvFun el orient (PrimState m) m
gemvAbstraction gemvName gemvSafeFFI gemvUnsafeFFI constHandler = gemv
  where
    shouldCallFast :: Int -> Int  -> Bool
    shouldCallFast a b = flopsThreshold >= gemvComplexity a b 
    gemv tr alpha beta
      (MutableDenseMatrix ornta ax ay astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
      (MutableDenseVector _ cdim cstride cbuff)
        | isBadGemv tr ax ay bdim cdim =  error $! "Bad dimension args to GEMV: ax ay xdim ydim: " ++ show [ax, ay, bdim, cdim]
        | SM.overlaps abuff cbuff || SM.overlaps bbuff cbuff =
            error $! "The read and write inputs for: " ++ gemvName ++ " overlap. This is a programmer error. Please fix." 
        | otherwise = call
            where
              (newx,newy) = coordSwapper tr (ax,ay)
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim bbuff $ \bp ->
                     unsafeWithPrim cbuff $ \cp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta  $ \betaPtr  ->
                       unsafePrimToPrim $! (if shouldCallFast newx newy  then gemvUnsafeFFI else gemvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFITranspose tr)
                         (fromIntegral newx) (fromIntegral newy) alphaPtr ap (fromIntegral astride) bp 
                         (fromIntegral bstride) betaPtr cp (fromIntegral cstride)

sgemv :: PrimMonad m => GemvFun Float orient (PrimState m) m
sgemv = gemvAbstraction "sgemv" cblas_sgemv_safe cblas_sgemv_unsafe (flip ($))

dgemv :: PrimMonad m => GemvFun Double orient (PrimState m) m
dgemv = gemvAbstraction "dgemv" cblas_dgemv_safe cblas_dgemv_unsafe (flip ($))

cgemv :: PrimMonad m => GemvFun (Complex Float) orient (PrimState m) m
cgemv = gemvAbstraction "cgemv" cblas_cgemv_safe cblas_cgemv_unsafe withRStorable_

zgemv :: PrimMonad m => GemvFun (Complex Double) orient (PrimState m) m
zgemv = gemvAbstraction "zgemv" cblas_zgemv_safe cblas_zgemv_unsafe withRStorable_

{-# NOINLINE trsvAbstraction #-}
trsvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TrsvFunFFI el -> TrsvFunFFI el
                -> forall orient . TrsvFun el orient (PrimState m) m
trsvAbstraction trsvName trsvSafeFFI trsvUnsafeFFI = trsv
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= (fromIntegral n)^2

    isBadTrsv :: Int -> Int -> Int -> Bool
    isBadTrsv nx ny vdim = nx < 0 || nx /= ny || nx /= vdim

    trsv uplo tra diag
      (MutableDenseMatrix ornt x y mstride mbuff)
      (MutableDenseVector _ vdim vstride vbuff)
        | isBadTrsv x y vdim =
            error $! "Bad dimension args to TRSV: x y vdim: " ++ show [x,y,vdim]
        | SM.overlaps vbuff mbuff =
            error $! "The read and write inputs for: " ++ trsvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = unsafeWithPrim mbuff $ \mp ->
                      unsafeWithPrim vbuff $ \vp ->
                        unsafePrimToPrim $! (if shouldCallFast x then trsvUnsafeFFI else trsvSafeFFI)
                          (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo) (encodeFFITranspose tra)
                          (encodeFFITriangleSort diag) (fromIntegral x) mp (fromIntegral mstride) vp
                          (fromIntegral vstride)

strsv :: PrimMonad m => TrsvFun Float orient (PrimState m) m
strsv = trsvAbstraction "strsv" cblas_strsv_safe cblas_strsv_unsafe

dtrsv :: PrimMonad m => TrsvFun Double orient (PrimState m) m
dtrsv = trsvAbstraction "dtrsv" cblas_dtrsv_safe cblas_dtrsv_unsafe

ctrsv :: PrimMonad m => TrsvFun (Complex Float) orient (PrimState m) m
ctrsv = trsvAbstraction "ctrsv" cblas_ctrsv_safe cblas_ctrsv_unsafe

ztrsv :: PrimMonad m => TrsvFun (Complex Double) orient (PrimState m) m
ztrsv = trsvAbstraction "ztrsv" cblas_ztrsv_safe cblas_ztrsv_unsafe
