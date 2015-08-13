{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level3(
    GemmFun
    ,HemmFun
    ,HerkFun
    ,Her2kFun
    ,SymmFun
    ,SyrkFun
    ,Syr2kFun

    ,gemmAbstraction
    ,hemmAbstraction
    ,herkAbstraction
    ,her2kAbstraction
    ,symmAbstraction
    ,syrkAbstraction
    ,syr2kAbstraction
    ) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level3
import Numerical.HBLAS.BLAS.Internal.Utility
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM
import Data.Int
import Foreign.Ptr

type GemmFun el orient s m = Transpose -> Transpose ->  el -> el  -> MDenseMatrix s orient el
  ->   MDenseMatrix s orient el  ->  MDenseMatrix s orient el -> m ()

type SymmFun el orient s m = EquationSide -> MatUpLo -> el -> el -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> MDenseMatrix s orient el -> m ()

type HemmFun el orient s m = EquationSide -> MatUpLo -> el -> el -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> MDenseMatrix s orient el -> m ()

type HerkFun scale el orient s m = MatUpLo -> Transpose -> scale -> scale -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> m ()

type Her2kFun scale el orient s m = MatUpLo -> Transpose -> el -> scale -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> MDenseMatrix s orient el -> m ()

type SyrkFun el orient s m = MatUpLo -> Transpose -> el -> el -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> m ()

type Syr2kFun el orient s m = MatUpLo -> Transpose -> el -> el -> MDenseMatrix s orient el
  -> MDenseMatrix s orient el -> MDenseMatrix s orient el -> m ()

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

{-# NOINLINE hemmAbstraction #-}
hemmAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> HemmFunFFI el -> HemmFunFFI el -> (el -> (Ptr el -> m ()) -> m ())
                -> forall orient . HemmFun el orient (PrimState m) m
hemmAbstraction hemmName hemmSafeFFI hemmUnsafeFFI constHandler = hemm
  where
    isBadHemmBothSide :: (Ord a, Num a) => a -> a -> a -> a -> a -> a -> Bool
    isBadHemmBothSide ax ay bx by cx cy = (minimum [ax, ay, bx, by, cx, cy] <= 0) || not (ax == ay && bx == cx && by == cy)

    isBadHemm :: (Ord a, Num a) => EquationSide -> a -> a -> a -> a -> a -> a -> Bool
    isBadHemm LeftSide ax ay bx by cx cy = isBadHemmBothSide ax ay bx by cx cy || (ax /= by)
    isBadHemm RightSide ax ay bx by cx cy = isBadHemmBothSide ax ay bx by cx cy || (bx /= ay)

    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast cy cx ax = flopsThreshold >= (fromIntegral cx :: Int64)
                                              * (fromIntegral cy :: Int64)
                                              * (fromIntegral ax :: Int64)

    hemm side uplo alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ bx by bstride bbuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadHemm side ax ay bx by cx cy = error $! "bad dimension args to hemm: ax ay bx by cx cy side: " ++ show [ax, ay, bx, by, cx ,cy] ++ " " ++ show side
            | SM.overlaps abuff cbuff || SM.overlaps bbuff cbuff =
                    error $ "the read and write inputs for: " ++ hemmName ++ " overlap. This is a programmer error. Please fix."
            | otherwise  =
                unsafeWithPrim abuff $ \ap ->
                unsafeWithPrim bbuff $ \bp ->
                unsafeWithPrim cbuff $ \cp  ->
                constHandler alpha $  \alphaPtr ->
                constHandler beta $ \betaPtr ->
                    do  let rawOrder = encodeNiceOrder ornta
                        let rawUplo = encodeFFIMatrixHalf uplo
                        let rawSide = encodeFFISide side
                        unsafePrimToPrim $!  (if shouldCallFast cy cx ax then hemmUnsafeFFI  else hemmSafeFFI)
                            rawOrder rawSide rawUplo (fromIntegral cy) (fromIntegral cx)
                                alphaPtr ap (fromIntegral astride) bp (fromIntegral bstride) betaPtr cp (fromIntegral cstride)

{-# NOINLINE herkAbstraction #-}
herkAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> HerkFunFFI scalePtr el -> HerkFunFFI scalePtr el -> (scale -> (scalePtr -> m ()) -> m ())
                -> forall orient . HerkFun scale el orient (PrimState m) m
herkAbstraction herkName herkSafeFFI herkUnsafeFFI constHandler = herk
  where
    isBadHerkBothSide :: (Ord a, Num a) => a -> a -> a -> a -> Bool
    isBadHerkBothSide ax ay cx cy = (minimum [ax, ay, cx, cy] <= 0) || (cx /= cy)

    isBadHerk :: (Ord a, Num a) => Transpose -> a -> a -> a -> a -> Bool
    isBadHerk NoTranspose ax ay cx cy = isBadHerkBothSide ax ay cx cy || (ay /= cx)
    isBadHerk ConjTranspose ax ay cx cy = isBadHerkBothSide ax ay cx cy || (ax /= cx)
    isBadHerk trans _ _ _ _ = error $ herkName ++ ": trans " ++ show trans ++ " is invalid."

    -- n * k * n
    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast ax ay cx = flopsThreshold >= (fromIntegral ax :: Int64)
                                              * (fromIntegral ay :: Int64)
                                              * (fromIntegral cx :: Int64)

    herk uplo trans alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadHerk trans ax ay cx cy = error $! "bad dimension args to " ++ herkName ++ ": ax ay cx cy side: " ++ show [ax, ay, cx ,cy] ++ " " ++ show trans
            | SM.overlaps abuff cbuff =
                    error $ "the read and write inputs for: " ++ herkName ++ " overlap. This is a programmer error. Please fix."
            | otherwise = call
                where
                  k = if (trans == NoTranspose) then ax else ay
                  call = unsafeWithPrim abuff $ \ap ->
                         unsafeWithPrim cbuff $ \cp  ->
                         constHandler alpha $  \alphaPtr ->
                         constHandler beta $ \betaPtr ->
                             do  let rawOrder = encodeNiceOrder ornta
                                 let rawUplo  = encodeFFIMatrixHalf uplo
                                 let rawTrans = encodeFFITranspose trans
                                 unsafePrimToPrim $!  (if shouldCallFast ax ay cx then herkUnsafeFFI  else herkSafeFFI)
                                     rawOrder rawUplo rawTrans (fromIntegral cy) (fromIntegral k)
                                         alphaPtr ap (fromIntegral astride) betaPtr cp (fromIntegral cstride)

{-# NOINLINE her2kAbstraction #-}
her2kAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> Her2kFunFFI scale el -> Her2kFunFFI scale el -> (el -> (Ptr el -> m ()) -> m ())
                -> forall orient . Her2kFun scale el orient (PrimState m) m
her2kAbstraction her2kName her2kSafeFFI her2kUnsafeFFI constHandler = her2k
  where
    isBadHer2kBothSide :: (Ord a, Num a) => a -> a -> a -> a -> a -> a -> Bool
    isBadHer2kBothSide ax ay bx by cx cy = (minimum [ax, ay, bx, by, cx, cy] <= 0) || not (cx == cy && ax == bx && ay == by)

    isBadHer2k :: (Ord a, Num a) => Transpose -> a -> a -> a -> a -> a -> a -> Bool
    isBadHer2k NoTranspose ax ay bx by cx cy = isBadHer2kBothSide ax ay bx by cx cy || (ay /= cx)
    isBadHer2k ConjTranspose ax ay bx by cx cy = isBadHer2kBothSide ax ay bx by cx cy || (ax /= cx)
    isBadHer2k trans _ _ _ _ _ _ = error $ her2kName ++ ": trans " ++ show trans ++ " is invalid."

    -- n * k * n
    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast ax ay cx = flopsThreshold >= (fromIntegral ax :: Int64)
                                              * (fromIntegral ay :: Int64)
                                              * (fromIntegral cx :: Int64)
                                              * 2

    her2k uplo trans alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ bx by bstride bbuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadHer2k trans ax ay bx by cx cy = error $! "bad dimension args to " ++ her2kName ++ ": ax ay cx cy side: " ++ show [ax, ay, bx, by, cx ,cy] ++ " " ++ show trans
            | SM.overlaps abuff cbuff =
                    error $ "the read and write inputs for: " ++ her2kName ++ " overlap. This is a programmer error. Please fix."
            | otherwise = call
                where
                  k = if (trans == NoTranspose) then ax else ay
                  call = unsafeWithPrim abuff $ \ap ->
                         unsafeWithPrim bbuff $ \bp  ->
                         unsafeWithPrim cbuff $ \cp  ->
                         constHandler alpha $  \alphaPtr ->
                             do  let rawOrder = encodeNiceOrder ornta
                                 let rawUplo  = encodeFFIMatrixHalf uplo
                                 let rawTrans = encodeFFITranspose trans
                                 unsafePrimToPrim $!  (if shouldCallFast ax ay cx then her2kUnsafeFFI  else her2kSafeFFI)
                                     rawOrder rawUplo rawTrans (fromIntegral cy) (fromIntegral k)
                                         alphaPtr ap (fromIntegral astride) bp (fromIntegral bstride) beta cp (fromIntegral cstride)

{-# NOINLINE syrkAbstraction #-}
syrkAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> SyrkFunFFI scale el -> SyrkFunFFI scale el -> (el -> (scale -> m ()) -> m ())
                -> forall orient . SyrkFun el orient (PrimState m) m
syrkAbstraction syrkName syrkSafeFFI syrkUnsafeFFI constHandler = syrk
  where
    isBadSyrkBothSide :: (Ord a, Num a) => a -> a -> a -> a -> Bool
    isBadSyrkBothSide ax ay cx cy = (minimum [ax, ay, cx, cy] <= 0) || (cx /= cy)

    isBadSyrk :: (Ord a, Num a) => Transpose -> a -> a -> a -> a -> Bool
    isBadSyrk NoTranspose ax ay cx cy = isBadSyrkBothSide ax ay cx cy || (ay /= cx)
    isBadSyrk Transpose ax ay cx cy = isBadSyrkBothSide ax ay cx cy || (ax /= cx)
    isBadSyrk ConjTranspose ax ay cx cy = isBadSyrkBothSide ax ay cx cy || (ax /= cx)
    isBadSyrk trans _ _ _ _ = error $ syrkName ++ ": trans " ++ show trans ++ " is invalid."

    -- n * k * n
    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast ax ay cx = flopsThreshold >= (fromIntegral ax :: Int64)
                                              * (fromIntegral ay :: Int64)
                                              * (fromIntegral cx :: Int64)

    syrk uplo trans alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadSyrk trans ax ay cx cy = error $! "bad dimension args to " ++ syrkName ++ ": ax ay cx cy side: " ++ show [ax, ay, cx ,cy] ++ " " ++ show trans
            | SM.overlaps abuff cbuff =
                    error $ "the read and write inputs for: " ++ syrkName ++ " overlap. This is a programmer error. Please fix."
            | otherwise = call
                where
                  k = if (trans == NoTranspose) then ax else ay
                  call = unsafeWithPrim abuff $ \ap ->
                         unsafeWithPrim cbuff $ \cp  ->
                         constHandler alpha $  \alphaPtr ->
                         constHandler beta $ \betaPtr ->
                             do  let rawOrder = encodeNiceOrder ornta
                                 let rawUplo  = encodeFFIMatrixHalf uplo
                                 let rawTrans = encodeFFITranspose trans
                                 unsafePrimToPrim $!  (if shouldCallFast ax ay cx then syrkUnsafeFFI  else syrkSafeFFI)
                                     rawOrder rawUplo rawTrans (fromIntegral cy) (fromIntegral k)
                                         alphaPtr ap (fromIntegral astride) betaPtr cp (fromIntegral cstride)

{-# NOINLINE syr2kAbstraction #-}
syr2kAbstraction :: (SM.Storable el, PrimMonad m)
                => String -> Syr2kFunFFI scale el -> Syr2kFunFFI scale el -> (el -> (scale -> m ()) -> m ())
                -> forall orient . Syr2kFun el orient (PrimState m) m
syr2kAbstraction syr2kName syr2kSafeFFI syr2kUnsafeFFI constHandler = syr2k
  where
    isBadSyr2kBothSide :: (Ord a, Num a) => a -> a -> a -> a -> a -> a -> Bool
    isBadSyr2kBothSide ax ay bx by cx cy = (minimum [ax, ay, bx, by, cx, cy] <= 0) || not (cx == cy && ax == bx && ay == by)

    isBadSyr2k :: (Ord a, Num a) => Transpose -> a -> a -> a -> a -> a -> a -> Bool
    isBadSyr2k NoTranspose ax ay bx by cx cy = isBadSyr2kBothSide ax ay bx by cx cy || (ay /= cx)
    isBadSyr2k Transpose ax ay bx by cx cy = isBadSyr2kBothSide ax ay bx by cx cy || (ax /= cx)
    isBadSyr2k ConjTranspose ax ay bx by cx cy = isBadSyr2kBothSide ax ay bx by cx cy || (ax /= cx)
    isBadSyr2k trans _ _ _ _ _ _ = error $ syr2kName ++ ": trans " ++ show trans ++ " is invalid."

    -- n * k * n
    shouldCallFast :: Int -> Int -> Int -> Bool
    shouldCallFast ax ay cx = flopsThreshold >= (fromIntegral ax :: Int64)
                                              * (fromIntegral ay :: Int64)
                                              * (fromIntegral cx :: Int64)
                                              * 2

    syr2k uplo trans alpha beta
        (MutableDenseMatrix ornta ax ay astride abuff)
        (MutableDenseMatrix _ bx by bstride bbuff)
        (MutableDenseMatrix _ cx cy cstride cbuff)
            | isBadSyr2k trans ax ay bx by cx cy = error $! "bad dimension args to " ++ syr2kName ++ ": ax ay cx cy side: " ++ show [ax, ay, bx, by, cx ,cy] ++ " " ++ show trans
            | SM.overlaps abuff cbuff =
                    error $ "the read and write inputs for: " ++ syr2kName ++ " overlap. This is a programmer error. Please fix."
            | otherwise = call
                where
                  k = if (trans == NoTranspose) then ax else ay
                  call = unsafeWithPrim abuff $ \ap ->
                         unsafeWithPrim bbuff $ \bp  ->
                         unsafeWithPrim cbuff $ \cp  ->
                         constHandler alpha $ \alphaPtr ->
                         constHandler beta $ \betaPtr ->
                             do  let rawOrder = encodeNiceOrder ornta
                                 let rawUplo  = encodeFFIMatrixHalf uplo
                                 let rawTrans = encodeFFITranspose trans
                                 unsafePrimToPrim $!  (if shouldCallFast ax ay cx then syr2kUnsafeFFI  else syr2kSafeFFI)
                                     rawOrder rawUplo rawTrans (fromIntegral cy) (fromIntegral k)
                                         alphaPtr ap (fromIntegral astride) bp (fromIntegral bstride) betaPtr cp (fromIntegral cstride)
