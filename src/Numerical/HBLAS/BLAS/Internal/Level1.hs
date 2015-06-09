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

type AsumFun el res s m = Int -> MDenseVector s Direct el -> Int -> m res
type AxpyFun el s m = Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> m()

{-# NOINLINE asumAbstraction #-}
asumAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AsumFunFFI el res -> AsumFunFFI el res ->
  AsumFun el res (PrimState m) m
asumAbstraction asumName asumSafeFFI asumUnsafeFFI = asum
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- for complex vector, 2n additions are needed
    isBad :: Int -> Int -> Int -> Bool
    isBad dim n incx = dim < (1 + (n-1) * incx)
    asum n (MutableDenseVector _ dim _ buff) incx
      | isBad dim n incx = error $! "Function " ++ asumName ++ ": matrix contains too few elements:" ++ show dim ++ " and " ++ show (1 + (n-1) * incx) ++ " elements are needed."
      | otherwise = unsafeWithPrim buff $ \ptr ->
        do unsafePrimToPrim $! (if shouldCallFast n then asumUnsafeFFI else asumSafeFFI) (fromIntegral n) ptr (fromIntegral incx)

{-# NOINLINE axpyAbstraction #-}
axpyAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AxpyFunFFI scale el -> AxpyFunFFI scale el -> (el -> (scale -> m()) -> m()) ->
  AxpyFun el (PrimState m) m
axpyAbstraction axpyName axpySafeFFI axpyUnsafeFFI constHandler = axpy
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- n for a*x, and n for +y
    isBad :: Int -> Int -> Int -> Bool
    isBad dim n incx = dim < (1 + (n-1) * incx)
    axpy n alpha
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
        | isBad adim n aincx = error $! "Function " ++ axpyName ++ ": first matrix contains too few elements:" ++ show adim ++ " and " ++ show (1 + (n-1) * aincx) ++ " elements are needed."
        | isBad bdim n bincx = error $! "Function " ++ axpyName ++ ": second matrix contains too few elements:" ++ show bdim ++ " and " ++ show (1 + (n-1) * bincx) ++ " elements are needed."
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          constHandler alpha $ \alphaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then axpyUnsafeFFI else axpySafeFFI) (fromIntegral n) alphaPtr ap (fromIntegral aincx) bp (fromIntegral bincx)

