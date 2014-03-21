{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal(
    GemmFun
    ,GemvFun
    ,GerFun
    ,TrsvFun 

    ,gemmAbstraction
    ,gemvAbstraction
    ,gerAbstraction
    ,trsvAbstraction
    ) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM
import Data.Int

type GemmFun el orient s m = Transpose ->Transpose ->  el -> el  -> MDenseMatrix s orient el
  ->   MDenseMatrix s orient el  ->  MDenseMatrix s orient el -> m ()

type GemvFun el orient s m = Transpose -> el -> el
  -> MDenseMatrix s orient el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m ()

type GerFun el orient s m =
    el -> MDenseVector s Direct el -> MDenseVector s Direct el -> MDenseMatrix s orient el -> m ()

type TrsvFun el orient s m =
      MatUpLo -> Transpose -> MatDiag
   -> MDenseMatrix s orient el -> MDenseVector s Direct el -> m ()

gemmComplexity :: Integral a => a -> a -> a -> Int64
gemmComplexity a b c = fromIntegral a * fromIntegral b *fromIntegral c  -- this will be wrong by some constant factor, albeit a small one

gemvComplexity :: Integral a => a -> a -> Int64
gemvComplexity a b = fromIntegral a * fromIntegral b


-- this covers the ~6 cases for checking the dimensions for GEMM quite nicely
isBadGemm :: (Ord a, Num a) =>
                   Transpose -> Transpose -> a -> a -> a -> a -> a -> a -> Bool
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
                    do  (axNew,_) <- return $ coordSwapper tra (ax,ay)
                        --- dont need to swap b, info is in a and c
                        --- c doesn't get implicitly transposed
                        let blasOrder = encodeNiceOrder ornta -- all three are the same orientation
                        let rawTra =  encodeFFITranspose tra
                        let rawTrb = encodeFFITranspose trb
                                 -- example of why i want to switch to singletones
                        unsafePrimToPrim $!  (if shouldCallFast cy cx axNew then gemmUnsafeFFI  else gemmSafeFFI )
                            blasOrder rawTra rawTrb (fromIntegral cy) (fromIntegral cx) (fromIntegral axNew)
                                alphaPtr ap  (fromIntegral astride) bp (fromIntegral bstride) betaPtr  cp (fromIntegral cstride)




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


{-# NOINLINE gerAbstraction #-}
gerAbstraction :: (SM.Storable el, PrimMonad m)
               => String
               -> GerFunFFI el
               -> GerFunFFI el
               -> forall orient . GerFun el orient (PrimState m) m
gerAbstraction gerName gerSafeFFI gerUnsafeFFI = ger
    where
      shouldCallFast :: Int -> Int -> Bool
      shouldCallFast m n = flopsThreshold >= (fromIntegral m :: Int64)
                                           * (fromIntegral n :: Int64)

      isBadGer :: Int -> Int -> Int -> Int -> Bool
      isBadGer dx dy ax ay = ax < 0 || ay < 0 || dx < ax || dy < ay

      ger alpha (MutableDenseVector _ xdim xstride xbuff)
                (MutableDenseVector _ ydim ystride ybuff)
                (MutableDenseMatrix ornta ax ay astride abuff)
        | isBadGer xdim ydim ax ay =
            error $! "bad dimension args to GER: xdim ydim ax ay" ++ show [xdim, ydim, ax, ay]
        | SM.overlaps xbuff abuff || SM.overlaps ybuff abuff =
            error $! "The read and write inputs for: " ++ gerName ++ " overlap. This is a programmer error. Please fix."
        | otherwise =
            unsafeWithPrim xbuff $ \xp ->
            unsafeWithPrim ybuff $ \yp ->
            unsafeWithPrim abuff $ \ap ->
                unsafePrimToPrim $! (if shouldCallFast ax ay then gerUnsafeFFI else gerSafeFFI)
                    (encodeNiceOrder ornta) (fromIntegral ax) (fromIntegral ay) alpha xp
                    (fromIntegral xstride) yp (fromIntegral ystride) ap (fromIntegral astride)

{-# NOINLINE trsvAbstraction #-}
trsvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TrsvFunFFI el -> TrsvFunFFI el
                -> forall orient . TrsvFun el orient (PrimState m) m
trsvAbstraction trsvName trsvSafeFFI trsvUnsafeFFI = trsv
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= (fromIntegral n :: Int64)^(2 :: Int64)

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

