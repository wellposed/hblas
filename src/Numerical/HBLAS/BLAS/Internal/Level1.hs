{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level1(
  AsumFun
  ,AxpyFun

  ,asumAbstraction
  ,axpyAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level1
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

type AsumFun el res orient s m = Int -> MDenseMatrix s orient el -> Int -> m res
type AxpyFun el orient s m = Int -> el -> MDenseMatrix s orient el -> Int -> MDenseMatrix s orient el -> Int -> m()

{-# NOINLINE asumAbstraction #-}
asumAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AsumFunFFI el res -> AsumFunFFI el res ->
  forall orient . AsumFun el res orient (PrimState m) m
asumAbstraction asumName asumSafeFFI asumUnsafeFFI = asum
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- for complex vector, 2n additions are needed
    isBad :: Int -> Int -> Int -> Int -> Bool
    isBad x y n incx = x * y < (1 + (n-1) * incx)
    asum n (MutableDenseMatrix _ x y _ buff) incx
      | isBad x y n incx = error $! "Function " ++ asumName ++ ": matrix contains too few elements:" ++ show [x, y] ++ " and " ++ show (1 + (n-1) * incx) ++ " elements are needed."
      | otherwise = unsafeWithPrim buff $ \ptr ->
        do unsafePrimToPrim $! (if shouldCallFast n then asumUnsafeFFI else asumSafeFFI) (fromIntegral n) ptr (fromIntegral incx)

{-# NOINLINE axpyAbstraction #-}
axpyAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AxpyFunFFI scale el -> AxpyFunFFI scale el -> (el -> (scale -> m()) -> m()) ->
  forall orient . AxpyFun el orient (PrimState m) m
axpyAbstraction axpyName axpySafeFFI axpyUnsafeFFI constHandler = axpy
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- n for a*x, and n for +y
    isBad :: Int -> Int -> Int -> Int -> Bool
    isBad x y n incx = x * y < (1 + (n-1) * incx)
    axpy n alpha
      (MutableDenseMatrix _ ax ay _ abuff) aincx
      (MutableDenseMatrix _ bx by _ bbuff) bincx
        | isBad ax ay n aincx = error $! "Function " ++ axpyName ++ ": first matrix contains too few elements:" ++ show [ax, ay] ++ " and " ++ show (1 + (n-1) * aincx) ++ " elements are needed."
        | isBad bx by n bincx = error $! "Function " ++ axpyName ++ ": second matrix contains too few elements:" ++ show [bx, by] ++ " and " ++ show (1 + (n-1) * bincx) ++ " elements are needed."
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          constHandler alpha $ \alphaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then axpyUnsafeFFI else axpySafeFFI) (fromIntegral n) alphaPtr ap (fromIntegral aincx) bp (fromIntegral bincx)

