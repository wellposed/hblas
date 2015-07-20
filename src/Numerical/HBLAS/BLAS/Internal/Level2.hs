{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level2(
  GbmvFun
  ,GemvFun

  ,gbmvAbstraction
  ,gemvAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level2
import Numerical.HBLAS.BLAS.Internal.Utility
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM
import Data.Int

-- In gbmv, only n is stored in a. So m and n are need to provided in the arguments.
type GbmvFun el orient s m = Transpose -> Int -> Int -> Int -> Int -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

type GemvFun el orient s m = Transpose -> el -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m ()

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

gemvComplexity :: Integral a => a -> a -> Int64
gemvComplexity a b = fromIntegral a * fromIntegral b

-- / checks if the size of a matrices rows matches input vector size
-- and the  column count matchesresult vector size
isBadGemv :: Transpose -> Int -> Int -> Int -> Int -> Bool
isBadGemv tr ax ay bdim cdim = isBadGemvHelper (cds tr (ax,ay))
    where
    cds = coordSwapper
    isBadGemvHelper (realX,realY)  =
            minimum [realY,realX,bdim,cdim] <= 0 ||  not (realX == bdim && realY == cdim )

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

