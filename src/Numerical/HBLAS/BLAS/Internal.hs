{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal(
    GemmFun
    ,SymmFun
    ,GerFun
    ,TrsvFun

    ,gemmAbstraction
    ,symmAbstraction
    ,gerAbstraction
    ,trsvAbstraction
    ) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.BLAS.FFI.Level2
import Numerical.HBLAS.BLAS.Internal.Utility
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM
import Data.Int

type GemmFun el orient s m = Transpose ->Transpose ->  el -> el  -> MDenseMatrix s orient el
  ->   MDenseMatrix s orient el  ->  MDenseMatrix s orient el -> m ()

type SymmFun el orient s m = EquationSide -> MatUpLo -> el -> el -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> MDenseMatrix s orient el -> m ()

type GerFun el orient s m =
    el -> MDenseVector s Direct el -> MDenseVector s Direct el -> MDenseMatrix s orient el -> m ()

type TrsvFun el orient s m =
      MatUpLo -> Transpose -> MatDiag
   -> MDenseMatrix s orient el -> MDenseVector s Direct el -> m ()

gemmComplexity :: Integral a => a -> a -> a -> Int64
gemmComplexity a b c = fromIntegral a * fromIntegral b *fromIntegral c  -- this will be wrong by some constant factor, albeit a small one


-- this covers the ~6 cases for checking the dimensions for GEMM quite nicely
isBadGemm :: (Ord a, Num a) =>
                   Transpose -> Transpose -> a -> a -> a -> a -> a -> a -> Bool
isBadGemm tra trb  ax ay bx by cx cy = isBadGemmHelper (cds tra (ax,ay)) (cds trb (bx,by) )  (cx,cy)
    where
    cds = coordSwapper
    isBadGemmHelper !(ax,ay) !(bx,by) !(cx,cy) =  (minimum [ax, ay, bx, by, cx ,cy] <= 0)
        || not (  cy ==  ay && cx == bx && ax == by)

isBadSymm :: (Ord a, Num a) =>
                   EquationSide -> a -> a -> a -> a -> a -> a -> Bool
isBadSymm LeftSide ax ay bx by cx cy = isBadSymmBothSide ax ay bx by cx cy
    || (ax /= by)
isBadSymm RightSide ax ay bx by cx cy = isBadSymmBothSide ax ay bx by cx cy
    || (bx /= ay)

isBadSymmBothSide :: (Ord a, Num a) => a -> a -> a -> a -> a -> a -> Bool
isBadSymmBothSide ax ay bx by cx cy = (minimum [ax, ay, bx, by, cx, cy] <= 0)
    || not (ax == ay && bx == cx && by == cy)

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

{-# NOINLINE symmAbstraction #-}
symmAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> SymmFunFFI scale el -> SymmFunFFI scale el -> (el -> (scale -> m ()) -> m ())
                -> forall orient . SymmFun el orient (PrimState m) m
symmAbstraction symmName symmSafeFFI symmUnsafeFFI constHandler = symm
  where
    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast cy cx ax = flopsThreshold >= (fromIntegral cx :: Int64)
                                              * (fromIntegral cy :: Int64)
                                              * (fromIntegral ax :: Int64)

    symm side uplo alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ bx by bstride bbuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadSymm side ax ay bx by cx cy = error $! "bad dimension args to SYMM: ax ay bx by cx cy side: " ++ show [ax, ay, bx, by, cx ,cy] ++ " " ++ show side
            | SM.overlaps abuff cbuff || SM.overlaps bbuff cbuff =
                    error $ "the read and write inputs for: " ++ symmName ++ " overlap. This is a programmer error. Please fix."
            | otherwise  =
                unsafeWithPrim abuff $ \ap ->
                unsafeWithPrim bbuff $ \bp ->
                unsafeWithPrim cbuff $ \cp  ->
                constHandler alpha $  \alphaPtr ->
                constHandler beta $ \betaPtr ->
                    do  let rawOrder = encodeNiceOrder ornta
                        let rawUplo = encodeFFIMatrixHalf uplo
                        let rawSide = encodeFFISide side
                        unsafePrimToPrim $!  (if shouldCallFast cy cx ax then symmUnsafeFFI  else symmSafeFFI)
                            rawOrder rawSide rawUplo (fromIntegral cy) (fromIntegral cx)
                                alphaPtr ap (fromIntegral astride) bp (fromIntegral bstride) betaPtr cp (fromIntegral cstride)

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

