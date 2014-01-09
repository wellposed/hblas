{-# LANGUAGE Trustworthy #-}
{- VERY TRUST WORTHY :) -}
module Numerical.OpenBLAS.UtilsFFI where


import  Data.Vector.Storable.Mutable  as  M 
import Control.Monad.Primitive
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe

import Foreign.Ptr

{-
the IO version of these various utils is in Base.
but would like to have the 
-}

withRWStorable:: (Storable a, PrimMonad m)=> a -> (Ptr a -> m b) -> m a 
withRWStorable val fun = do 
    valVect <- M.replicate 1 val 
    _ <- unsafeWithPrim valVect fun 
    M.unsafeRead valVect 0 
{-# INLINE withRWStorable #-}    


withRStorable :: (Storable a, PrimMonad m)=> a -> (Ptr a -> m b) -> m b 
withRStorable val fun = do   
    valVect <- M.replicate 1 val 
    unsafeWithPrim valVect fun 
{-# INLINE withRStorable #-} 

withForeignPtrPrim :: PrimMonad m => ForeignPtr a -> (Ptr a -> m b) -> m b
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
unsafeWithPrim (MVector _ fp)  fun = withForeignPtrPrim fp fun 
