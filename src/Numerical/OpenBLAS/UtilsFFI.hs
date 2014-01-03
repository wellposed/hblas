{-# LANGUAGE Trustworthy #-}
module Numerical.OpenBLAS.UtilsFFI where


import  Data.Vector.Storable.Mutable  as  M 
import Control.Monad.Primitive
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe

import Foreign.Ptr

withRWStorable:: (Storable a, PrimMonad m)=> a -> (Ptr a -> m b) -> m a 
withRWStorable val fun = do 
    valVect <- M.replicate 1 val 
    _ <- unsafeWithPrim valVect fun 
    M.read valVect 0 
{-# INLINE withRWStorable #-}    


withRStorable :: (Storable a, PrimMonad m)=> a -> (Ptr a -> m b) -> m b 
withRStorable val fun = do   
    valVect <- M.replicate 1 val 
    unsafeWithPrim valVect fun 
{-# INLINE withRStorable #-} 

withForeignPtrPrim :: PrimMonad m => ForeignPtr a -> (Ptr a -> m b) -> m b
-- ^This is a way to look at the pointer living inside a
-- foreign object.  This function takes a function which is
-- applied to that pointer. The resulting 'IO' action is then
-- executed. The foreign object is kept alive at least during
-- the whole action, even if it is not used directly
-- inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses
-- of the pointer should be inside the
-- 'withForeignPtr' bracket.  The reason for
-- this unsafeness is the same as for
-- 'unsafeForeignPtrToPtr' below: the finalizer
-- may run earlier than expected, because the compiler can only
-- track usage of the 'ForeignPtr' object, not
-- a 'Ptr' object made from it.
--
-- This function is normally used for marshalling data to
-- or from the object pointed to by the
-- 'ForeignPtr', using the operations from the
-- 'Storable' class.
withForeignPtrPrim  fo act
  = do r <- act (unsafeForeignPtrToPtr fo)
       touchForeignPtrPrim fo
       return r
{-# INLINE withForeignPtrPrim#-}       

touchForeignPtrPrim ::PrimMonad m => ForeignPtr a -> m ()
touchForeignPtrPrim fp = unsafePrimToPrim $!  touchForeignPtr fp
{-# INLINE touchForeignPtrPrim #-}




unsafeWithPrim ::( Storable a, PrimMonad m )=> MVector (PrimState m) a -> (Ptr a -> m b) -> m b
{-# INLINE unsafeWithPrim #-}
unsafeWithPrim (MVector _ fp) = withForeignPtrPrim fp
