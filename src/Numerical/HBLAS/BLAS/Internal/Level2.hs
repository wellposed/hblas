{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level2(
  GbmvFun
  ,GemvFun
  ,GerFun
  ,HbmvFun
  ,HemvFun
  ,HerFun
  ,Her2Fun

  ,gbmvAbstraction
  ,gemvAbstraction
  ,gerAbstraction
  ,hbmvAbstraction
  ,hemvAbstraction
  ,herAbstraction
  ,her2Abstraction
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

type GerFun el orient s m = el -> MDenseVector s Direct el -> MDenseVector s Direct el -> MDenseMatrix s orient el -> m ()

type HbmvFun el orient s m = MatUpLo -> Int -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

type HemvFun el orient s m = MatUpLo -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

type HerFun scale el orient s m = MatUpLo -> scale -> MDenseVector s Direct el -> Int -> MDenseMatrix s orient el -> m ()

type Her2Fun el orient s m = MatUpLo -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MDenseMatrix s orient el -> m ()

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
      (MutableDenseVector _ xdim _ xbuff) incx
      beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim srcVecLen incx = error $! vectorBadInfo gbmvName "input vector" xdim srcVecLen incx
        | isVectorBadWithNIncrement ydim dstVecLen incy = error $! vectorBadInfo gbmvName "output vector" ydim dstVecLen incy
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim xbuff $ \xp ->
          unsafeWithPrim ybuff $ \yp ->
          constHandler alpha $ \alphaPtr ->
          constHandler beta $ \betaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast ax ay then gbmvUnsafeFFI else gbmvSafeFFI) (encodeNiceOrder ornta) (encodeFFITranspose trans) (fromIntegral m) (fromIntegral n) (fromIntegral kl) (fromIntegral ku) alphaPtr ap (fromIntegral astride) xp (fromIntegral incx) betaPtr yp (fromIntegral incy)
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

{-# NOINLINE gerAbstraction #-}
gerAbstraction :: (SM.Storable el, PrimMonad m)
               => String
               -> GerxFunFFI scale el
               -> GerxFunFFI scale el
               -> (el -> (scale -> m ())-> m ())
               -> forall orient . GerFun el orient (PrimState m) m
gerAbstraction gerName gerSafeFFI gerUnsafeFFI constHandler = ger
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
            error $! "bad dimension args to " ++ gerName ++ ": xdim ydim ax ay" ++ show [xdim, ydim, ax, ay]
        | SM.overlaps xbuff abuff || SM.overlaps ybuff abuff =
            error $! "The read and write inputs for: " ++ gerName ++ " overlap. This is a programmer error. Please fix."
        | otherwise =
            unsafeWithPrim xbuff $ \xp ->
            unsafeWithPrim ybuff $ \yp ->
            unsafeWithPrim abuff $ \ap ->
            constHandler alpha $ \alphaPtr ->
                unsafePrimToPrim $! (if shouldCallFast ax ay then gerUnsafeFFI else gerSafeFFI)
                    (encodeNiceOrder ornta) (fromIntegral ax) (fromIntegral ay) alphaPtr xp
                    (fromIntegral xstride) yp (fromIntegral ystride) ap (fromIntegral astride)

-- uplo is upper
-- [ a b c 0 0 0]
-- [ 0 d e f 0 0]
-- [ 0 0 g h i 0]
-- [ 0 0 0 j k l]
-- [ 0 0 0 0 m n]
-- [ 0 0 0 0 0 o]
--
-- changed to a:
--
-- [ 0 0 c f i l]
-- [ 0 b e h k n]
-- [ a d g j m o]
--
--
-- uplo is lower
-- [ a 0 0 0 0 0]
-- [ b c 0 0 0 0]
-- [ d e f 0 0 0]
-- [ 0 g h i 0 0]
-- [ 0 0 j k l 0]
-- [ 0 0 0 m n o]
--
-- changed to a:
--
-- [ a c f i l o]
-- [ b e h k n 0]
-- [ d g j m 0 0]
{-# NOINLINE hbmvAbstraction #-}
hbmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> HbmvFunFFI scale el
                -> HbmvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . HbmvFun el orient (PrimState m) m
hbmvAbstraction hbmvName hbmvSafeFFI hbmvUnsafeFFI constHandler = hbmv
  where
    shouldCallFast :: Int -> Int  -> Bool
    shouldCallFast a b = flopsThreshold >= (fromIntegral a) * (fromIntegral b)
    hbmv uplo k alpha
      (MutableDenseMatrix ornta ax ay astride abuff)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim ay incx = error $! vectorBadInfo hbmvName "x vector" xdim ay incx
        | isVectorBadWithNIncrement ydim ay incy = error $! vectorBadInfo hbmvName "y vector" ydim ay incy
        | astride < k + 1 = error $! hbmvName ++ ": lda " ++ (show astride) ++ " should be greater than k " ++ (show k) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ hbmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta  $ \betaPtr  ->
                       unsafePrimToPrim $! (if shouldCallFast ax ay then hbmvUnsafeFFI else hbmvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral ay) (fromIntegral k) alphaPtr ap (fromIntegral astride) xp
                         (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE hemvAbstraction #-}
hemvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> HemvFunFFI scale el
                -> HemvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . HemvFun el orient (PrimState m) m
hemvAbstraction hemvName hemvSafeFFI hemvUnsafeFFI constHandler = hemv
  where
    shouldCallFast :: Int -> Int  -> Bool
    shouldCallFast a b = flopsThreshold >= (fromIntegral a) * (fromIntegral b)
    hemv uplo alpha
      (MutableDenseMatrix ornta ax ay astride abuff)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim ay incx = error $! vectorBadInfo hemvName "x vector" xdim ay incx
        | isVectorBadWithNIncrement ydim ay incy = error $! vectorBadInfo hemvName "y vector" ydim ay incy
        | astride < ay = error $! hemvName ++ ": lda " ++ (show astride) ++ " should be greater than or equal with n " ++ (show ay) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ hemvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta  $ \betaPtr  ->
                       unsafePrimToPrim $! (if shouldCallFast ax ay then hemvUnsafeFFI else hemvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral ay) alphaPtr ap (fromIntegral astride) xp
                         (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE herAbstraction #-}
herAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> HerFunFFI scalePtr el
                -> HerFunFFI scalePtr el
                -> (scale -> (scalePtr -> m ())-> m ())
                -> forall orient . HerFun scale el orient (PrimState m) m
herAbstraction herName herSafeFFI herUnsafeFFI constHandler = her
  where
    shouldCallFast :: Int -> Int  -> Bool
    shouldCallFast a b = flopsThreshold >= (fromIntegral a) * (fromIntegral b)
    her uplo alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseMatrix ornta _ ay astride abuff)
        | isVectorBadWithNIncrement xdim ay incx = error $! vectorBadInfo herName "x vector" xdim ay incx
        | astride < ay = error $! herName ++ ": lda " ++ (show astride) ++ " should be greater than or equal with n " ++ (show ay) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ herName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast ay ay then herUnsafeFFI else herSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral ay) alphaPtr xp (fromIntegral incx) ap (fromIntegral astride)

{-# NOINLINE her2Abstraction #-}
her2Abstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> Her2FunFFI scale el
                -> Her2FunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . Her2Fun el orient (PrimState m) m
her2Abstraction her2Name her2SafeFFI her2UnsafeFFI constHandler = her2
  where
    shouldCallFast :: Int -> Int  -> Bool
    shouldCallFast a b = flopsThreshold >= (fromIntegral a) * (fromIntegral b)
    her2 uplo alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ ydim _ ybuff) incy
      (MutableDenseMatrix ornta _ ay astride abuff)
        | isVectorBadWithNIncrement xdim ay incx = error $! vectorBadInfo her2Name "x vector" xdim ay incx
        | isVectorBadWithNIncrement ydim ay incy = error $! vectorBadInfo her2Name "y vector" ydim ay incy
        | astride < ay = error $! her2Name ++ ": lda " ++ (show astride) ++ " should be greater than or equal with n " ++ (show ay) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ her2Name ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast ay ay then her2UnsafeFFI else her2SafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral ay) alphaPtr yp (fromIntegral incy) xp (fromIntegral incx) ap (fromIntegral astride)
