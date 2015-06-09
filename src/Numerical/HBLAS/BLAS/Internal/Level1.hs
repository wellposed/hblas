{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level1(
  AsumFun

  ,asumAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level1
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

type AsumFun el res orient s m = Int -> MDenseMatrix s orient el -> Int -> m res

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
      | isBad x y n incx = error $! "Function " ++ asumName ++ ": matrix contains too few elements:" ++ show [x, y] ++ " and " ++ show (1 + (n-1) * incx) ++ " are needed."
      | otherwise = unsafeWithPrim buff $ \ptr ->
        do unsafePrimToPrim $! (if shouldCallFast n then asumUnsafeFFI else asumSafeFFI) (fromIntegral n) ptr (fromIntegral incx)
