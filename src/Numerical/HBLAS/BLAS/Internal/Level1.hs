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
  ,RotmgFun
  ,ScalFun
  ,SwapFun
  ,IamaxFun
  --,IaminFun

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
  ,rotmgAbstraction
  ,scalAbstraction
  ,swapAbstraction
  ,iamaxAbstraction
  --,iaminAbstraction
) where

import Numerical.HBLAS.Constants
import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level1
import Numerical.HBLAS.BLAS.Internal.Utility
import Numerical.HBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.C.Types
import Foreign.Ptr

type AsumFun el s m res = Int -> MDenseVector s Direct el -> m res
type AxpyFun el s m = Int -> el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m()
type CopyFun el s m = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> m()
type NoScalarDotFun el s m res = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> m res
type ScalarDotFun el s m res = Int -> el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m res
type ComplexDotFun el s m = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> MValue (PrimState m) el -> m()
type Nrm2Fun el s m res = Int -> MDenseVector s Direct el -> m res
type RotFun el s m = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> el -> el -> m()
type RotgFun el s m = MValue (PrimState m) el -> MValue (PrimState m) el -> MValue (PrimState m) el -> MValue (PrimState m) el -> m()
type RotmFun el s m = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> MDenseVector s Direct el -> m()
type RotmgFun el s m = MValue (PrimState m) el -> MValue (PrimState m) el -> MValue (PrimState m) el -> el -> MDenseVector s Direct el -> m()
type ScalFun scale el s m = Int -> scale -> MDenseVector s Direct el -> m()
type SwapFun el s m = Int -> MDenseVector s Direct el -> MDenseVector s Direct el -> m()
type IamaxFun el s m = Int -> MDenseVector s Direct el -> m Int
--type IaminFun el s m = Int -> MDenseVector s Direct el -> Int -> m Int

{-# NOINLINE asumAbstraction #-}
asumAbstraction:: (SM.Storable el, PrimMonad m) => String ->
  AsumFunFFI el res -> AsumFunFFI el res ->
  AsumFun el (PrimState m) m res
asumAbstraction asumName asumSafeFFI asumUnsafeFFI = asum
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- for complex vector, 2n additions are needed
    asum n (MutableDenseVector _ dim stride buff)
      | isVectorBadWithNIncrement dim n stride = error $! vectorBadInfo asumName "source matrix" dim n stride
      | otherwise = unsafeWithPrim buff $ \ptr ->
        do unsafePrimToPrim $! (if shouldCallFast n then asumUnsafeFFI else asumSafeFFI) (fromIntegral n) ptr (fromIntegral stride)

{-# NOINLINE axpyAbstraction #-}
axpyAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  AxpyFunFFI scale el -> AxpyFunFFI scale el -> (el -> (scale -> m()) -> m()) ->
  AxpyFun el (PrimState m) m
axpyAbstraction axpyName axpySafeFFI axpyUnsafeFFI constHandler = axpy
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= 2 * (fromIntegral n) -- n for a*x, and n for +y
    axpy n alpha
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo axpyName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo axpyName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          constHandler alpha $ \alphaPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then axpyUnsafeFFI else axpySafeFFI) (fromIntegral n) alphaPtr ap (fromIntegral astride) bp (fromIntegral bstride)

{-# NOINLINE copyAbstraction #-}
copyAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  CopyFunFFI el -> CopyFunFFI el ->
  CopyFun el (PrimState m) m
copyAbstraction copyName copySafeFFI copyUnsafeFFI = copy
  where
    shouldCallFast :: Bool
    shouldCallFast = True -- TODO:(yjj) to confirm no flops are needed in copy
    copy n
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo copyName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo copyName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast then copyUnsafeFFI else copySafeFFI) (fromIntegral n) ap (fromIntegral astride) bp (fromIntegral bstride)

{-# NOINLINE noScalarDotAbstraction #-}
noScalarDotAbstraction :: (SM.Storable el, PrimMonad m) => String ->
  NoScalarDotFunFFI el res -> NoScalarDotFunFFI el res ->
  NoScalarDotFun el (PrimState m) m res
noScalarDotAbstraction dotName dotSafeFFI dotUnsafeFFI = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo dotName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo dotName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) (fromIntegral n) ap (fromIntegral astride) bp (fromIntegral bstride)

{-# NOINLINE scalarDotAbstraction #-}
scalarDotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  SdsdotFortranFunFFI el res -> SdsdotFortranFunFFI el res -> (CInt -> (Ptr CInt -> m res) -> m res) -> (el -> (Ptr el -> m res) -> m res) ->
  ScalarDotFun el (PrimState m) m res
scalarDotAbstraction dotName dotSafeFFI dotUnsafeFFI intConstHandler scaleConstHandler = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n sb
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo dotName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo dotName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          intConstHandler (fromIntegral n) $ \nPtr ->
          intConstHandler (fromIntegral astride) $ \incaPtr ->
          intConstHandler (fromIntegral bstride) $ \incbPtr ->
          scaleConstHandler sb $ \sbPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) nPtr sbPtr ap incaPtr bp incbPtr

{-# NOINLINE complexDotAbstraction #-}
complexDotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  ComplexDotFunFFI el -> ComplexDotFunFFI el ->
  ComplexDotFun el (PrimState m) m
complexDotAbstraction dotName dotSafeFFI dotUnsafeFFI = dot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    dot n
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
      (MutableValue resbuff)
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo dotName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo dotName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
          unsafeWithPrim resbuff $ \resPtr ->
            do unsafePrimToPrim $! (if shouldCallFast n then dotUnsafeFFI else dotSafeFFI) (fromIntegral n) ap (fromIntegral astride) bp (fromIntegral bstride) resPtr

{-# NOINLINE norm2Abstraction #-}
norm2Abstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  Nrm2FunFFI el res -> Nrm2FunFFI el res ->
  Nrm2Fun el (PrimState m) m res
norm2Abstraction norm2Name norm2SafeFFI norm2UnsafeFFI = norm2
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n -- not sure, maybe for complex is 2n
    norm2 n
      (MutableDenseVector _ dim stride buff)
        | isVectorBadWithNIncrement dim n stride = error $! vectorBadInfo norm2Name "input matrix" dim n stride
        | otherwise =
          unsafeWithPrim buff $ \p ->
            do unsafePrimToPrim $! (if shouldCallFast n then norm2UnsafeFFI else norm2SafeFFI) (fromIntegral n) p (fromIntegral stride)

{-# NOINLINE rotAbstraction #-}
rotAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  RotFunFFI el -> RotFunFFI el ->
  RotFun el (PrimState m) m
rotAbstraction rotName rotSafeFFI rotUnsafeFFI = rot
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    rot n
      (MutableDenseVector _ adim astride abuff)
      (MutableDenseVector _ bdim bstride bbuff)
      c s
        | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo rotName "first matrix" adim n astride
        | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo rotName "second matrix" bdim n bstride
        | otherwise =
          unsafeWithPrim abuff $ \ap ->
          unsafeWithPrim bbuff $ \bp ->
            do unsafePrimToPrim $! (if shouldCallFast n then rotUnsafeFFI else rotSafeFFI) (fromIntegral n) ap (fromIntegral astride) bp (fromIntegral bstride) c s

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
    rotm n (MutableDenseVector _ adim astride abuff)
           (MutableDenseVector _ bdim bstride bbuff)
           (MutableDenseVector _ pdim _ pbuff)
      | isVectorBadWithNIncrement adim n astride = error $! vectorBadInfo rotmName "first matrix" adim n astride
      | isVectorBadWithNIncrement bdim n bstride = error $! vectorBadInfo rotmName "second matrix" bdim n bstride
      | pdim /= 5 = error $! rotmName ++ " param dimension is not 5"
      | otherwise =
        unsafeWithPrim abuff $ \ap ->
        unsafeWithPrim bbuff $ \bp ->
        unsafeWithPrim pbuff $ \pp ->
          do unsafePrimToPrim $! (if shouldCallFast then rotmUnsafeFFI else rotmSafeFFI) (fromIntegral n) ap (fromIntegral astride) bp (fromIntegral bstride) pp

{-# NOINLINE rotmgAbstraction #-}
rotmgAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  RotmgFunFFI el -> RotmgFunFFI el ->
  RotmgFun el (PrimState m) m
rotmgAbstraction rotmgName rotmgSafeFFI rotmgUnsafeFFI = rotmg
  where
    shouldCallFast :: Bool
    shouldCallFast = True -- O(1)
    rotmg (MutableValue d1)
          (MutableValue d2)
          (MutableValue x1)
          y1
          (MutableDenseVector _ pdim _ pbuff)
      | pdim /= 5 = error $! rotmgName ++ " param dimension is not 5"
      | otherwise =
        unsafeWithPrim d1 $ \d1p ->
        unsafeWithPrim d2 $ \d2p ->
        unsafeWithPrim x1 $ \x1p ->
        unsafeWithPrim pbuff $ \pp ->
          do unsafePrimToPrim $! (if shouldCallFast then rotmgUnsafeFFI else rotmgSafeFFI) d1p d2p x1p y1 pp

{-# NOINLINE scalAbstraction #-}
scalAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  ScalFunFFI scale el -> ScalFunFFI scale el -> (scaleplain -> (scale -> m()) -> m()) ->
  ScalFun scaleplain el (PrimState m) m
scalAbstraction scalName scalSafeFFI scalUnsafeFFI constHandler = scal
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n
    scal n alpha (MutableDenseVector _ xdim stride xbuff)
      | isVectorBadWithNIncrement xdim n stride = error $! vectorBadInfo scalName "vector" xdim n stride
      | otherwise =
        unsafeWithPrim xbuff $ \xptr ->
        constHandler alpha $ \alphaPtr ->
          do unsafePrimToPrim $! (if shouldCallFast n then scalUnsafeFFI else scalSafeFFI) (fromIntegral n) alphaPtr xptr (fromIntegral stride)

{-# NOINLINE swapAbstraction #-}
swapAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  SwapFunFFI el -> SwapFunFFI el ->
  SwapFun el (PrimState m) m
swapAbstraction swapName swapSafeFFI swapUnsafeFFI = swap
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n -- no computation? only n times memory access?
    swap n (MutableDenseVector _ xdim xstride xbuff)
      (MutableDenseVector _ ydim ystride ybuff)
      | isVectorBadWithNIncrement xdim n xstride = error $! vectorBadInfo swapName "vector x" xdim n xstride
      | isVectorBadWithNIncrement ydim n ystride = error $! vectorBadInfo swapName "vector y" ydim n ystride
      | otherwise =
        unsafeWithPrim xbuff $ \xptr ->
        unsafeWithPrim ybuff $ \yptr ->
          do unsafePrimToPrim $! (if shouldCallFast n then swapUnsafeFFI else swapSafeFFI) (fromIntegral n) xptr (fromIntegral xstride) yptr (fromIntegral ystride)

{-# NOINLINE iamaxAbstraction #-}
iamaxAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  IamaxFunFFI el -> IamaxFunFFI el ->
  IamaxFun el (PrimState m) m
iamaxAbstraction iamaxName iamaxSafeFFI iamaxUnsafeFFI = iamax
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n -- n times comparison
    iamax n (MutableDenseVector _ xdim xstride xbuff)
      | isVectorBadWithNIncrement xdim n xstride = error $! vectorBadInfo iamaxName "target vector" xdim n xstride
      | otherwise =
        unsafeWithPrim xbuff $ \xptr ->
          do unsafePrimToPrim $! (if shouldCallFast n then iamaxUnsafeFFI else iamaxSafeFFI) (fromIntegral n) xptr (fromIntegral xstride)

{-
{-# NOINLINE iaminAbstraction #-}
iaminAbstraction :: (SM.Storable el, PrimMonad m, Show el) => String ->
  IaminFunFFI el -> IaminFunFFI el ->
  IaminFun el (PrimState m) m
iaminAbstraction iaminName iaminSafeFFI iaminUnsafeFFI = iamin
  where
    shouldCallFast :: Int -> Bool
    shouldCallFast n = flopsThreshold >= fromIntegral n -- n times comparison
    iamin n (MutableDenseVector _ xdim _ xbuff) xincx
      | isVectorBadWithNIncrement xdim n xincx = error $! vectorBadInfo iaminName "target vector" xdim n xincx
      | otherwise =
        unsafeWithPrim xbuff $ \xptr ->
          do unsafePrimToPrim $! (if shouldCallFast n then iaminUnsafeFFI else iaminSafeFFI) (fromIntegral n) xptr (fromIntegral xincx)
-}
