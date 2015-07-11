{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

module Numerical.HBLAS.BLAS.Internal.Level1(
  AsumFun
  ,AxpyFun
  ,CopyFun
  ,NoScalarDotFun
  ,ScalarDotFun
  ,ComplexDotFun
  ,Nrm2Fun
  ,RotFun
  ,RotgFun
  ,RotmFun

  ,asumAbstraction
  ,axpyAbstraction
  ,copyAbstraction
  ,noScalarDotAbstraction
  ,scalarDotAbstraction
  ,complexDotAbstraction
  ,norm2Abstraction
  ,rotAbstraction
  ,rotgAbstraction
  ,rotmAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level1
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

type AsumFun el s m res = Int -> MDenseVector s Direct el -> Int -> m res
type AxpyFun el s m = Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> m()
type CopyFun el s m = Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> m()
type NoScalarDotFun el s m res = Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> m res
type ScalarDotFun el s m res = Int -> el -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> m res
type ComplexDotFun el s m = Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MValue (PrimState m) el -> m()
type Nrm2Fun el s m res = Int -> MDenseVector s Direct el -> Int -> m res
type RotFun el s m = Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> el -> el -> m()
type RotgFun el s m = MValue (PrimState m) el -> MValue (PrimState m) el -> MValue (PrimState m) el -> MValue (PrimState m) el -> m()
type RotmFun el s m = Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> Int -> MDenseVector s Direct el -> m()

isVectorBadWithNIncrement :: Int -> Int -> Int -> Bool
isVectorBadWithNIncrement dim n incx = dim < (1 + (n-1) * incx)

vectorBadInfo :: String -> String -> Int -> Int -> Int -> String
vectorBadInfo funName matName dim n incx = "Function " ++ funName ++ ": " ++ matName ++ " constains too few elements of " ++ show dim ++ " and " ++ show (1 + (n-1) * incx) ++ " elements are needed."

{-# NOINLINE asumAbstraction #-}
asumAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AsumFunFFI el res -> AsumFunFFI el res ->
  AsumFun el (PrimState m) m res
asumAbstraction asumName asumSafeFFI asumUnsafeFFI = asum
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- for complex vector, 2n additions are needed
    asum n (MutableDenseVector _ dim _ buff) incx
      | isVectorBadWithNIncrement dim n incx = error $! vectorBadInfo asumName "source matrix" dim n incx
      | otherwise = unsafeWithPrim buff $ \ptr ->
        do unsafePrimToPrim $! (if shouldCallFast n then asumUnsafeFFI else asumSafeFFI) (fromIntegral n) ptr (fromIntegral incx)

{-# NOINLINE axpyAbstraction #-}
axpyAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  AxpyFunFFI scale el -> AxpyFunFFI scale el -> (el -> (scale -> m()) -> m()) ->
  AxpyFun el (PrimState m) m
axpyAbstraction axpyName axpySafeFFI axpyUnsafeFFI constHandler = axpy
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- n for a*x, and n for +y
    axpy n alpha
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo axpyName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo axpyName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          constHandler alpha $ \alphaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then axpyUnsafeFFI else axpySafeFFI) (fromIntegral n) alphaPtr ap (fromIntegral aincx) bp (fromIntegral bincx)

{-# NOINLINE copyAbstraction #-}
copyAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  CopyFunFFI el -> CopyFunFFI el ->
  CopyFun el (PrimState m) m
copyAbstraction copyName copySafeFFI copyUnsafeFFI = copy
  where
    shouldCallFast :: Bool
    shouldCallFast = True -- TODO:(yjj) to confirm no flops are needed in copy
    copy n
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo copyName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo copyName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast then copyUnsafeFFI else copySafeFFI) (fromIntegral n) ap (fromIntegral aincx) bp (fromIntegral bincx)

{-# NOINLINE noScalarDotAbstraction #-}
noScalarDotAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  NoScalarDotFunFFI el res -> NoScalarDotFunFFI el res ->
  NoScalarDotFun el (PrimState m) m res
noScalarDotAbstraction dotName dotSafeFFI dotUnsafeFFI = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo dotName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo dotName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) (fromIntegral n) ap (fromIntegral aincx) bp (fromIntegral bincx)

{-# NOINLINE scalarDotAbstraction #-}
scalarDotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  ScalarDotFunFFI el res -> ScalarDotFunFFI el res ->
  ScalarDotFun el (PrimState m) m res
scalarDotAbstraction dotName dotSafeFFI dotUnsafeFFI = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n sb
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo dotName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo dotName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) (fromIntegral n) sb ap (fromIntegral aincx) bp (fromIntegral bincx)

{-# NOINLINE complexDotAbstraction #-}
complexDotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  ComplexDotFunFFI el -> ComplexDotFunFFI el ->
  ComplexDotFun el (PrimState m) m
complexDotAbstraction dotName dotSafeFFI dotUnsafeFFI = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
      (MutableValue resbuff)
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo dotName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo dotName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          unsafeWithPrim resbuff $ \resPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) (fromIntegral n) ap (fromIntegral aincx) bp (fromIntegral bincx) resPtr

{-# NOINLINE norm2Abstraction #-}
norm2Abstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  Nrm2FunFFI el res -> Nrm2FunFFI el res ->
  Nrm2Fun el (PrimState m) m res
norm2Abstraction norm2Name norm2SafeFFI norm2UnsafeFFI = norm2
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n -- not sure, maybe for complex is 2n
    norm2 n
      (MutableDenseVector _ dim _ buff) incx
        | isVectorBadWithNIncrement dim n incx = error $! vectorBadInfo norm2Name "input matrix" dim n incx
        | otherwise =
          unsafeWithPrim buff $ \p ->
            do unsafePrimToPrim $! (if shouldCallFast n then norm2UnsafeFFI else norm2SafeFFI) (fromIntegral n) p (fromIntegral incx)

{-# NOINLINE rotAbstraction #-}
rotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  RotFunFFI el -> RotFunFFI el ->
  RotFun el (PrimState m) m
rotAbstraction rotName rotSafeFFI rotUnsafeFFI = rot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    rot n
      (MutableDenseVector _ adim _ abuff) aincx
      (MutableDenseVector _ bdim _ bbuff) bincx
      c s
        | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo rotName "first matrix" adim n aincx
        | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo rotName "second matrix" bdim n bincx
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast n then rotUnsafeFFI else rotSafeFFI) (fromIntegral n) ap (fromIntegral aincx) bp (fromIntegral bincx) c s

{-# NOINLINE rotgAbstraction #-}
rotgAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  RotgFunFFI el -> RotgFunFFI el ->
  RotgFun el (PrimState m) m
rotgAbstraction rotgName rotgSafeFFI rotgUnsafeFFI = rotg
  where
    shouldCallFast :: Bool
    shouldCallFast = True -- not sure, seems O(1)
    rotg (MutableValue aptr) (MutableValue bptr) (MutableValue cptr) (MutableValue sptr)
      = unsafeWithPrim aptr $ \ap ->
        unsafeWithPrim bptr $ \bp ->
        unsafeWithPrim cptr $ \cp ->
        unsafeWithPrim sptr $ \sp ->
         do unsafePrimToPrim $! (if shouldCallFast then rotgUnsafeFFI else rotgSafeFFI) ap bp cp sp

{-# NOINLINE rotmAbstraction #-}
rotmAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  RotmFunFFI el -> RotmFunFFI el ->
  RotmFun el (PrimState m) m
rotmAbstraction rotmName rotmSafeFFI rotmUnsafeFFI = rotm
  where
    shouldCallFast :: Bool
    shouldCallFast = True -- O(1)
    rotm n (MutableDenseVector _ adim _ abuff) aincx
           (MutableDenseVector _ bdim _ bbuff) bincx
           (MutableDenseVector _ pdim _ pbuff)
      | isVectorBadWithNIncrement adim n aincx = error $! vectorBadInfo rotmName "first matrix" adim n aincx
      | isVectorBadWithNIncrement bdim n bincx = error $! vectorBadInfo rotmName "second matrix" bdim n bincx
      | pdim /= 5 = error $! rotmName ++ " param dimension is not 5"
      | otherwise =
        unsafeWithPrim abuff $ \ap ->
        unsafeWithPrim bbuff $ \bp ->
        unsafeWithPrim pbuff $ \pp ->
          do unsafePrimToPrim $! (if shouldCallFast then rotmUnsafeFFI else rotmSafeFFI) (fromIntegral n) ap (fromIntegral aincx) bp (fromIntegral bincx) pp
