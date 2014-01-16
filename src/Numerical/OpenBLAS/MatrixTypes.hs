
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-#  LANGUAGE GADTs #-}





module Numerical.OpenBLAS.MatrixTypes where

import Data.Vector.Storable as S 
import Data.Vector.Storable as SM
import Control.Monad.Primitive  
-- import Control.Monad.Primitive

{-| PSA, the matrix data types used in the hOpenBLAS binding
should not be regarded as being general purpose matrices.

They are designed to exactly express only the matrices which are 
valid inputs for BLAS. When applicable, such matrices should be easily mapped 
to and from other matrix libraries. That said,
the BLAS and LAPACK matrix formats capture a rich and very expressive subset
of Dense Matrix formats.

The primary and hence default format is Dense Row and Column Major Matrices,
but support will be added for other formats that BLAS and LAPACK provide operations for.

A guiding rule of thumb for this package is that there are no generic abstractions
provided, merely machinery to ensure all uses of BLAS and LAPACK operations
can be used in their full generality in a human friendly type safe fashion.
It is the role of a higher leve library to provide any generic operations.

-}    

data Orientation = Row | Column 


type family Transpose (x :: Orientation) :: Orientation

type instance Transpose Row = Column
type instance Transpose Column = Row 

-- | 'DenseMatrix' is for dense row or column major
data DenseMatrix :: Orientation -> * -> *  where 
    RowMajorDenseMatrix :: {-# UNPACK #-}!Int -> {-# UNPACK #-}!Int ->
                        {-# UNPACK #-} !Int -> !(S.Vector elem) -> DenseMatrix Row elem 

    ColMajorDenseMatrix :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int ->
                      {-# UNPACK #-}!Int -> !(S.Vector elem) -> DenseMatrix Column elem 

-- | 'MDenseMatrix' 
data MDenseMatrix :: *  ->Orientation  -> * -> *  where 
    RowMajorMutableDenseMatrix :: {-# UNPACK #-}!Int -> {-# UNPACK #-}!Int ->{-# UNPACK #-} !Int ->
                                 !(SM.MVector s  elem) -> MDenseMatrix s Row elem

    ColMajorMutableDenseMatrix :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int ->
                          {-# UNPACK #-}!Int -> !(SM.MVector s elem) -> MDenseMatrix s Column elem 

unsafeFreezeDenseMatrix :: (Storable elem, PrimMonad m)=> MDenseMatrix (PrimState m) or elem -> m (DenseMatrix or elem)
unsafeFreezeDenseMatrix (RowMajorMutableDenseMatrix  a b c mv) = do
        v <- S.unsafeFreeze mv 
        return $! RowMajorDenseMatrix a b c v                          
unsafeFreezeDenseMatrix (ColMajorMutableDenseMatrix a b c mv)=do
        v <- S.unsafeFreeze mv 
        return $! ColMajorDenseMatrix a b c v 


unsafeThawDenseMatrix :: (Storable elem, PrimMonad m)=> DenseMatrix or elem-> m (MDenseMatrix (PrimState m) or elem)  
unsafeThawDenseMatrix (RowMajorDenseMatrix a b c v) = do 
        mv <- S.unsafeThaw v
        return $! RowMajorMutableDenseMatrix a b c mv 
unsafeThawDenseMatrix (ColMajorDenseMatrix a b c v) = do 
        mv <- S.unsafeThaw v 
        return $! ColMajorMutableDenseMatrix a b c mv 

 

getDenseMatrixRow :: DenseMatrix or elem -> Int
getDenseMatrixRow (RowMajorDenseMatrix _ ydim _ _)= ydim
getDenseMatrixRow (ColMajorDenseMatrix _ ydim _ _) = ydim

getDenseMatrixColumn ::  DenseMatrix or elem -> Int
getDenseMatrixColumn (RowMajorDenseMatrix xdim _ _ _)= xdim
getDenseMatrixColumn (ColMajorDenseMatrix xdim _ _ _) = xdim

getDenseMatrixLeadingDimStride :: DenseMatrix or elem -> Int 
getDenseMatrixLeadingDimStride (RowMajorDenseMatrix _ _ stride _ ) = stride
getDenseMatrixLeadingDimStride (ColMajorDenseMatrix _ _ stride _ )= stride

getDenseMatrixArray :: DenseMatrix or elem -> S.Vector elem 
getDenseMatrixArray (RowMajorDenseMatrix _ _ _ arr) = arr
getDenseMatrixArray (ColMajorDenseMatrix _ _ _ arr) = arr 


uncheckedDenseMatrixIndex :: (S.Storable elem )=>  DenseMatrix or elem -> (Int,Int) -> elem 
uncheckedDenseMatrixIndex (RowMajorDenseMatrix _ _ ystride arr) =  \ (x,y)-> arr S.! (x + y * ystride)
uncheckedDenseMatrixIndex (ColMajorDenseMatrix _ _ xstride arr) = \ (x,y)-> arr S.! (y + x* xstride)



--- this (uncheckedMatrixSlice) will need to have its inlining quality checked


--- | slice over matrix element in the range (inclusive)  [xstart..xend] X [ystart .. yend]
--- call as  @'uncheckedMatrixSlice' matrix (xstart,ystart) (xend,yend) @
uncheckedDenseMatrixSlice :: (S.Storable elem)=>  DenseMatrix or elem -> (Int,Int)-> (Int,Int)-> DenseMatrix or elem 
uncheckedDenseMatrixSlice (RowMajorDenseMatrix xdim _ ystride arr) (xstart,ystart) (xend,yend) = res
    where   !res = RowMajorDenseMatrix (xend - xstart + 1) --  X : n - 0 + 1, because zero indexed
                                (yend - ystart+1)   -- Y : m - 0 + 1, because zero indexed
                                (ystride + xstart + (xdim - xend)) -- how much start and end padding per row
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (xstart+ystart*ystride)
            !ixEnd = (xend+yend*ystride)            
uncheckedDenseMatrixSlice (ColMajorDenseMatrix _ ydim xstride arr)  (xstart,ystart) (xend,yend) =  res
    where   !res = ColMajorDenseMatrix (xend - xstart + 1) 
                                (yend - ystart+1) 
                                (xstride + ystart + (ydim - yend))
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (ystart+xstart*xstride)
            !ixEnd = (yend+xend*xstride)

-- | tranposeMatrix does a shallow transpose that swaps the format and the x y params, but changes nothing
-- in the memory layout. 
-- Most applications where transpose is used in a computation need a deep, copying, tranpose operation
transposeDenseMatrix :: (inor ~ (Transpose outor) ,   outor ~ (Transpose inor)  ) =>   DenseMatrix inor elem -> DenseMatrix outor elem 
transposeDenseMatrix (RowMajorDenseMatrix x y stride arr)= (ColMajorDenseMatrix y x stride arr)
transposeDenseMatrix (ColMajorDenseMatrix x y stride arr) =(RowMajorDenseMatrix y x stride arr)


{-
need an init with index/ map with index, etc utils≤

-}



