{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level2(
  GbmvFun
  ,GemvFun
  ,GerFun
  ,HbmvFun
  ,HemvFun
  ,HerFun
  ,Her2Fun
  ,HpmvFun
  ,HprFun
  ,Hpr2Fun
  ,SbmvFun
  ,SpmvFun
  ,SprFun
  ,Spr2Fun
  ,SymvFun
  ,SyrFun
  ,Syr2Fun
  ,TbmvFun
  ,TbsvFun
  ,TpmvFun
  ,TpsvFun
  ,TrmvFun
  ,TrsvFun

  ,gbmvAbstraction
  ,gemvAbstraction
  ,gerAbstraction
  ,hbmvAbstraction
  ,hemvAbstraction
  ,herAbstraction
  ,her2Abstraction
  ,hpmvAbstraction
  ,hprAbstraction
  ,hpr2Abstraction
  ,sprAbstraction
  ,spr2Abstraction
  ,sbmvAbstraction
  ,spmvAbstraction
  ,symvAbstraction
  ,syrAbstraction
  ,syr2Abstraction
  ,tbmvAbstraction
  ,tbsvAbstraction
  ,tpmvAbstraction
  ,tpsvAbstraction
  ,trmvAbstraction
  ,trsvAbstraction
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

type HpmvFun el orient s m = SOrientation orient -> MatUpLo -> Int -> el -> MDenseVector s Direct el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

type HprFun scale el orient s m = SOrientation orient -> MatUpLo -> Int -> scale -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> m()

type Hpr2Fun el orient s m = SOrientation orient -> MatUpLo -> Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> m()

type SbmvFun el orient s m = MatUpLo -> Int -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m()

type SpmvFun el orient s m = SOrientation orient -> MatUpLo -> Int -> el -> MDenseVector s Direct el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m()

type SprFun el orient s m = SOrientation orient -> MatUpLo -> Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> m ()

type Spr2Fun el orient s m = SOrientation orient -> MatUpLo -> Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> m ()

type SymvFun el orient s m = MatUpLo -> el -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> el -> MDenseVector s Direct el -> Int -> m ()

type SyrFun el orient s m = MatUpLo -> el -> MDenseVector s Direct el -> Int -> MDenseMatrix s orient el -> m ()

type Syr2Fun el orient s m = MatUpLo -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MDenseMatrix s orient el -> m ()

type TbmvFun el orient s m = MatUpLo -> Transpose -> MatDiag -> Int -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> m ()

type TbsvFun el orient s m = MatUpLo -> Transpose -> MatDiag -> Int -> MDenseMatrix s orient el -> MDenseVector s Direct el -> Int -> m ()

type TpmvFun el orient s m = SOrientation orient -> MatUpLo -> Transpose -> MatDiag -> Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> Int -> m ()

type TpsvFun el orient s m = SOrientation orient -> MatUpLo -> Transpose -> MatDiag -> Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> Int -> m ()

type TrmvFun el orient s m =  MatUpLo -> Transpose -> MatDiag -> MDenseMatrix s orient el -> MDenseVector s Direct el -> m ()

type TrsvFun el orient s m =  MatUpLo -> Transpose -> MatDiag -> MDenseMatrix s orient el -> MDenseVector s Direct el -> m ()

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
      (MutableDenseMatrix ornta ax ay _ abuff) -- (n, lda)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo hbmvName "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo hbmvName "y vector" ydim n incy
        | lda < k + 1 = error $! hbmvName ++ ": lda " ++ (show lda) ++ " should be greater than k " ++ (show k) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ hbmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta  $ \betaPtr  ->
                       unsafePrimToPrim $! (if shouldCallFast n k then hbmvUnsafeFFI else hbmvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) (fromIntegral k) alphaPtr ap (fromIntegral lda) xp
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
                         (fromIntegral ay) alphaPtr xp (fromIntegral incx) yp (fromIntegral incy) ap (fromIntegral astride)

{-# NOINLINE hpmvAbstraction #-}
hpmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> HpmvFunFFI scale el
                -> HpmvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . HpmvFun el orient (PrimState m) m
hpmvAbstraction hpmvName hpmvSafeFFI hpmvUnsafeFFI constHandler = hpmv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n + n)
    hpmv ornt uplo n alpha
      (MutableDenseVector _ adim _ abuff)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo hpmvName "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo hpmvName "y vector" ydim n incy
        | adim < (div (n * (n+1)) 2) = error $! hpmvName ++ ": array must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ hpmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta $ \betaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then hpmvUnsafeFFI else hpmvSafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr ap xp (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE hprAbstraction #-}
hprAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> HprFunFFI scalePtr el
                -> HprFunFFI scalePtr el
                -> (scale -> (scalePtr -> m ())-> m ())
                -> forall orient . HprFun scale el orient (PrimState m) m
hprAbstraction hprName hprSafeFFI hprUnsafeFFI constHandler = hpr
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n + n)
    hpr ornt uplo n alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ adim _ abuff)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo hprName "x vector" xdim n incx
        | adim < (div (n * (n+1)) 2) = error $! hprName ++ ": array must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ hprName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then hprUnsafeFFI else hprSafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) ap

{-# NOINLINE hpr2Abstraction #-}
hpr2Abstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> Hpr2FunFFI scale el
                -> Hpr2FunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . Hpr2Fun el orient (PrimState m) m
hpr2Abstraction hpr2Name hpr2SafeFFI hpr2UnsafeFFI constHandler = hpr2
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n + n)
    hpr2 ornt uplo n alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ ydim _ ybuff) incy
      (MutableDenseVector _ adim _ abuff)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo hpr2Name "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo hpr2Name "y vector" ydim n incy
        | adim < (div (n * (n+1)) 2) = error $! hpr2Name ++ ": array must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ hpr2Name ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then hpr2UnsafeFFI else hpr2SafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) yp (fromIntegral incy) ap

{-# NOINLINE sbmvAbstraction #-}
sbmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> SbmvFunFFI scale el
                -> SbmvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . SbmvFun el orient (PrimState m) m
sbmvAbstraction sbmvName sbmvSafeFFI sbmvUnsafeFFI constHandler = sbmv
  where
    shouldCallFast :: Int -> Int -> Bool
    shouldCallFast n k = flopsThreshold >= (fromIntegral n) * (fromIntegral k)
    sbmv uplo k alpha
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo sbmvName "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo sbmvName "y vector" ydim n incy
        | lda < k+1 = error $! sbmvName ++ ": lda (" ++ (show lda) ++ ") must be greater than k (" ++ (show k) ++ ") + 1."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ sbmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta $ \betaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast n k then sbmvUnsafeFFI else sbmvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) (fromIntegral k) alphaPtr ap (fromIntegral lda) xp (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE spmvAbstraction #-}
spmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> SpmvFunFFI scale el
                -> SpmvFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . SpmvFun el orient (PrimState m) m
spmvAbstraction spmvName spmvSafeFFI spmvUnsafeFFI constHandler = spmv
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= (fromIntegral n) * (fromIntegral n)
    spmv ornta uplo n alpha
      (MutableDenseVector _ adim _ abuff)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo spmvName "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo spmvName "y vector" ydim n incy
        | adim < (div (n * (n+1)) 2) = error $! spmvName ++ ": array which has" ++ (show adim) ++ " elements must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ spmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta $ \betaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast n then spmvUnsafeFFI else spmvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr ap xp (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE sprAbstraction #-}
sprAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> SprFunFFI scale el
                -> SprFunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . SprFun el orient (PrimState m) m
sprAbstraction sprName sprSafeFFI sprUnsafeFFI constHandler = spr
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n)
    spr ornt uplo n alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ adim _ abuff)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo sprName "x vector" xdim n incx
        | adim < (div (n * (n+1)) 2) = error $! sprName ++ ": array which has" ++ (show adim) ++ " elements must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ sprName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then sprUnsafeFFI else sprSafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) ap

{-# NOINLINE spr2Abstraction #-}
spr2Abstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> Spr2FunFFI scale el
                -> Spr2FunFFI scale el
                -> (el -> (scale -> m ())-> m ())
                -> forall orient . Spr2Fun el orient (PrimState m) m
spr2Abstraction spr2Name spr2SafeFFI spr2UnsafeFFI constHandler = spr2
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n * 2)
    spr2 ornt uplo n alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ ydim _ ybuff) incy
      (MutableDenseVector _ adim _ abuff)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo spr2Name "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo spr2Name "y vector" ydim n incy
        | adim < (div (n * (n+1)) 2) = error $! spr2Name ++ ": array which has" ++ (show adim) ++ " elements must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ spr2Name ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then spr2UnsafeFFI else spr2SafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) yp (fromIntegral incy) ap

{-# NOINLINE symvAbstraction #-}
symvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> SymvFunFFI el
                -> SymvFunFFI el
                -> (el -> (el -> m ())-> m ())
                -> forall orient . SymvFun el orient (PrimState m) m
symvAbstraction symvName symvSafeFFI symvUnsafeFFI constHandler = symv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n)
    symv uplo alpha
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
      (MutableDenseVector _ xdim _ xbuff) incx beta
      (MutableDenseVector _ ydim _ ybuff) incy
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo symvName "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo symvName "y vector" ydim n incy
        | lda < n = error $! symvName ++ ": lda (" ++ (show lda) ++ ") must be greater than or equal to n (" ++ (show n) ++ ")."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ symvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                     constHandler beta $ \betaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then symvUnsafeFFI else symvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr ap (fromIntegral lda) xp (fromIntegral incx) betaPtr yp (fromIntegral incy)

{-# NOINLINE syrAbstraction #-}
syrAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> SyrFunFFI el
                -> SyrFunFFI el
                -> (el -> (el -> m ())-> m ())
                -> forall orient . SyrFun el orient (PrimState m) m
syrAbstraction syrName syrSafeFFI syrUnsafeFFI constHandler = syr
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n)
    syr uplo alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo syrName "x vector" xdim n incx
        | lda < n = error $! syrName ++ ": lda (" ++ (show lda) ++ ") must be greater than or equal to n (" ++ (show n) ++ ")."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ syrName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then syrUnsafeFFI else syrSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) ap (fromIntegral lda)

{-# NOINLINE syr2Abstraction #-}
syr2Abstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> Syr2FunFFI el
                -> Syr2FunFFI el
                -> (el -> (el -> m ())-> m ())
                -> forall orient . Syr2Fun el orient (PrimState m) m
syr2Abstraction syr2Name syr2SafeFFI syr2UnsafeFFI constHandler = syr2
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n * 2)
    syr2 uplo alpha
      (MutableDenseVector _ xdim _ xbuff) incx
      (MutableDenseVector _ ydim _ ybuff) incy
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo syr2Name "x vector" xdim n incx
        | isVectorBadWithNIncrement ydim n incy = error $! vectorBadInfo syr2Name "y vector" ydim n incy
        | lda < n = error $! syr2Name ++ ": lda (" ++ (show lda) ++ ") must be greater than or equal to n (" ++ (show n) ++ ")."
        | SM.overlaps abuff xbuff || SM.overlaps abuff ybuff || SM.overlaps xbuff ybuff =
            error $! "The read and write inputs for: " ++ syr2Name ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                     unsafeWithPrim ybuff $ \yp ->
                     constHandler alpha $ \alphaPtr ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then syr2UnsafeFFI else syr2SafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo)
                         (fromIntegral n) alphaPtr xp (fromIntegral incx) yp (fromIntegral incy) ap (fromIntegral lda)

-- To get x=A*x.
{-# NOINLINE tbmvAbstraction #-}
tbmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TbmvFunFFI el
                -> TbmvFunFFI el
                -> forall orient . TbmvFun el orient (PrimState m) m
tbmvAbstraction tbmvName tbmvSafeFFI tbmvUnsafeFFI = tbmv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n)
    tbmv uplo trans diag k
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
      (MutableDenseVector _ xdim _ xbuff) incx
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo tbmvName "x vector" xdim n incx
        | lda < k + 1 = error $! tbmvName ++ ": lda " ++ (show lda) ++ " should be greater than k " ++ (show k) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ tbmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then tbmvUnsafeFFI else tbmvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo) (encodeFFITranspose trans) (encodeFFITriangleSort diag)
                         (fromIntegral n) (fromIntegral k) ap (fromIntegral lda) xp (fromIntegral incx)

-- To solve A*x = b to get x.
{-# NOINLINE tbsvAbstraction #-}
tbsvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TbsvFunFFI el
                -> TbsvFunFFI el
                -> forall orient . TbsvFun el orient (PrimState m) m
tbsvAbstraction tbsvName tbsvSafeFFI tbsvUnsafeFFI = tbsv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n) -- TODO: to confirm the computation.
    tbsv uplo trans diag k
      (MutableDenseMatrix ornta ax ay _ abuff)  -- (n, lda)
      (MutableDenseVector _ xdim _ xbuff) incx
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo tbsvName "x vector" xdim n incx
        | lda < k + 1 = error $! tbsvName ++ ": lda " ++ (show lda) ++ " should be greater than k " ++ (show k) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ tbsvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              n = ax
              lda = ay
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then tbsvUnsafeFFI else tbsvSafeFFI)
                         (encodeNiceOrder ornta) (encodeFFIMatrixHalf uplo) (encodeFFITranspose trans) (encodeFFITriangleSort diag)
                         (fromIntegral n) (fromIntegral k) ap (fromIntegral lda) xp (fromIntegral incx)

-- To get x=A*x. A is array of (n * (n+1) / 2) length
{-# NOINLINE tpmvAbstraction #-}
tpmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TpmvFunFFI el
                -> TpmvFunFFI el
                -> forall orient . TpmvFun el orient (PrimState m) m
tpmvAbstraction tpmvName tpmvSafeFFI tpmvUnsafeFFI = tpmv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n)
    tpmv ornt uplo trans diag n
      (MutableDenseVector _ adim _ abuff)
      (MutableDenseVector _ xdim _ xbuff) incx
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo tpmvName "x vector" xdim n incx
        | adim < (div (n * (n+1)) 2) = error $! tpmvName ++ ": array which has" ++ (show adim) ++ " elements must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ tpmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then tpmvUnsafeFFI else tpmvSafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo) (encodeFFITranspose trans) (encodeFFITriangleSort diag)
                         (fromIntegral n) ap xp (fromIntegral incx)

-- To solve A*x=b to get x. A is array of (n * (n+1) / 2) length
{-# NOINLINE tpsvAbstraction #-}
tpsvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TpsvFunFFI el
                -> TpsvFunFFI el
                -> forall orient . TpsvFun el orient (PrimState m) m
tpsvAbstraction tpsvName tpsvSafeFFI tpsvUnsafeFFI = tpsv
  where
    shouldCallFast :: Int64 -> Bool
    shouldCallFast n = flopsThreshold >= (n * n) -- TODO: to confirm the computation.
    tpsv ornt uplo trans diag n
      (MutableDenseVector _ adim _ abuff)
      (MutableDenseVector _ xdim _ xbuff) incx
        | isVectorBadWithNIncrement xdim n incx = error $! vectorBadInfo tpsvName "x vector" xdim n incx
        | adim < (div (n * (n+1)) 2) = error $! tpsvName ++ ": array which has" ++ (show adim) ++ " elements must contain at least (n*(n+1)/2) elements with n:" ++ (show n) ++ "."
        | SM.overlaps abuff xbuff =
            error $! "The read and write inputs for: " ++ tpsvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = call
            where
              call = unsafeWithPrim abuff $ \ap ->
                     unsafeWithPrim xbuff $ \xp ->
                       unsafePrimToPrim $! (if shouldCallFast (fromIntegral n) then tpsvUnsafeFFI else tpsvSafeFFI)
                         (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo) (encodeFFITranspose trans) (encodeFFITriangleSort diag)
                         (fromIntegral n) ap xp (fromIntegral incx)

{-# NOINLINE trmvAbstraction #-}
trmvAbstraction :: (SM.Storable el, PrimMonad m)
                => String
                -> TrmvFunFFI el -> TrmvFunFFI el
                -> forall orient . TrmvFun el orient (PrimState m) m
trmvAbstraction trmvName trmvSafeFFI trmvUnsafeFFI = trmv
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= (fromIntegral n :: Int64)^(2 :: Int64)

    isBadtrmv :: Int -> Int -> Int -> Bool
    isBadtrmv nx ny vdim = nx < 0 || nx /= ny || nx /= vdim

    trmv uplo tra diag
      (MutableDenseMatrix ornt x y mstride mbuff)
      (MutableDenseVector _ vdim vstride vbuff)
        | isBadtrmv x y vdim =
            error $! "Bad dimension args to trmv: x y vdim: " ++ show [x,y,vdim]
        | SM.overlaps vbuff mbuff =
            error $! "The read and write inputs for: " ++ trmvName ++ " overlap. This is a programmer error. Please fix."
        | otherwise = unsafeWithPrim mbuff $ \mp ->
                      unsafeWithPrim vbuff $ \vp ->
                        unsafePrimToPrim $! (if shouldCallFast x then trmvUnsafeFFI else trmvSafeFFI)
                          (encodeNiceOrder ornt) (encodeFFIMatrixHalf uplo) (encodeFFITranspose tra)
                          (encodeFFITriangleSort diag) (fromIntegral x) mp (fromIntegral mstride) vp
                          (fromIntegral vstride)

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

