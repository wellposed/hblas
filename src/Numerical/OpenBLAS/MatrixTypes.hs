
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-#  LANGUAGE GADTs #-}



-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}


module Numerical.OpenBLAS.MatrixTypes where

import Data.Vector.Storable as S 
--import Data.Vector.Storable as SV 
-- import Control.Monad.Primitive

{-| PSA, the matrix data types used in the hOpenBLAS binding
should not be regarded as being general purpose matrices.

They are designed to exactly express only the matrices which are 
valid inputs for BLAS. When applicable, such matrices should be easily mapped 
to and from other matrix libraries. 
-}    

data Orientation = Row | Column 


type family Transpose (x :: Orientation) :: Orientation

type instance Transpose Row = Column
type instance Transpose Column = Row 

data Matrix :: Orientation -> * -> *  where 
    RowMajorMatrix :: {-# UNPACK #-}!Int -> {-# UNPACK #-}!Int ->{-# UNPACK #-} !Int -> !(S.Vector elem) -> Matrix Row elem 
    ColMajorMatrix :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int ->  {-# UNPACK #-}!Int -> !(S.Vector elem) -> Matrix Column elem 

getMatrixRow :: Matrix or elem -> Int
getMatrixRow (RowMajorMatrix _ ydim _ _)= ydim
getMatrixRow (ColMajorMatrix _ ydim _ _) = ydim

getMatrixColumn ::  Matrix or elem -> Int
getMatrixColumn (RowMajorMatrix xdim _ _ _)= xdim
getMatrixColumn (ColMajorMatrix xdim _ _ _) = xdim

getMatrixLeadingDimStride :: Matrix or elem -> Int 
getMatrixLeadingDimStride (RowMajorMatrix _ _ stride _ ) = stride
getMatrixLeadingDimStride (ColMajorMatrix _ _ stride _ )= stride

getMatrixArray :: Matrix or elem -> S.Vector elem 
getMatrixArray (RowMajorMatrix _ _ _ arr) = arr
getMatrixArray (ColMajorMatrix _ _ _ arr) = arr 


uncheckedMatrixIndex :: (S.Storable elem )=> Matrix or elem -> (Int,Int) -> elem 
uncheckedMatrixIndex (RowMajorMatrix _ _ ystride arr) = \ (x,y)-> arr S.! (x + y * ystride)
uncheckedMatrixIndex (ColMajorMatrix _ _ xstride arr) = \ (x,y)-> arr S.! (y + x* xstride)



--- this (uncheckedMatrixSlice) will need to have its inlining quality checked


--- | slice over matrix element in the range (inclusive)  [xstart..xend] X [ystart .. yend]
--- call as  uncheckedMatrixSlice matrix (xstart,ystart) (xend,yend)
uncheckedMatrixSlice :: (S.Storable elem)=> Matrix or elem -> (Int,Int)-> (Int,Int)-> Matrix or elem 
uncheckedMatrixSlice (RowMajorMatrix xdim _ ystride arr) (xstart,ystart) (xend,yend) = res
    where   !res = RowMajorMatrix (xend - xstart + 1) 
                                (yend - ystart+1) 
                                (ystride + xstart + (xdim - xend))
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (xstart+ystart*ystride)
            !ixEnd = (xend+yend*ystride)
uncheckedMatrixSlice (ColMajorMatrix _ ydim xstride arr)  (xstart,ystart) (xend,yend) =  res
    where   !res = ColMajorMatrix (xend - xstart + 1) 
                                (yend - ystart+1) 
                                (xstride + ystart + (ydim - yend))
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (ystart+xstart*xstride)
            !ixEnd = (yend+xend*xstride)

transposeMatrix :: (inor ~ (Transpose outor) , outor ~ (Transpose inor)  ) => Matrix inor elem -> Matrix outor elem 
transposeMatrix (RowMajorMatrix x y stride arr)= (ColMajorMatrix x y stride arr)
transposeMatrix (ColMajorMatrix x y stride arr) =(RowMajorMatrix x y stride arr)

