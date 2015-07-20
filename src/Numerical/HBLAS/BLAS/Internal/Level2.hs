{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level2(
  GbmvFun

  ,gbmvAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level2
import Numerical.HBLAS.BLAS.Internal.Utility
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

-- In gbmv, only n is stored in a. So m and n are need to provided in the arguments.
type GbmvFun el orient s m = Transpose -> Int -> Int -> Int -> Int -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

{-# NOINLINE gbmvAbstraction #-}
gbmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> GbmvFunFFI scale el
                -> GbmvFunFFI scale el
                -> (el -> (scale -> m()) -> m())
                -> forall orient . GbmvFun el orient (PrimState m) m
gbmvAbstraction gbmvName gbmvSafeFFI gbmvUnsafeFFI constHandler = gbmv
  where
    shouldCallFast :: Int -> Int -> Bool -- we need ax * ay multiplications
    shouldCallFast a b = flopsThreshold >= (fromIntegral a) * (fromIntegral b)

    gbmv trans m n kl ku alpha
      (MutableDenseMatrix ornta ax ay astride abuff)
      (MutableDenseVector _ xdim _ xbuff) xincx
      beta
      (MutableDenseVector _ ydim _ ybuff) yincx
        | isVectorBadWithNIncrement xdim srcVecLen xincx = error $! vectorBadInfo gbmvName "input vector" xdim srcVecLen xincx
        | isVectorBadWithNIncrement ydim dstVecLen yincx = error $! vectorBadInfo gbmvName "output vector" ydim dstVecLen yincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim xbuff $ \xp ->
          unsafeWithPrim ybuff $ \yp ->
          constHandler alpha $ \alphaPtr ->
          constHandler beta $ \betaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast ax ay then gbmvUnsafeFFI else gbmvSafeFFI) (encodeNiceOrder ornta) (encodeFFITranspose trans) (fromIntegral m) (fromIntegral n) (fromIntegral kl) (fromIntegral ku) alphaPtr ap (fromIntegral astride) xp (fromIntegral xincx) betaPtr yp (fromIntegral yincx)
        where srcVecLen = snd $ coordSwapper trans (m, n)
              dstVecLen = fst $ coordSwapper trans (m, n)
