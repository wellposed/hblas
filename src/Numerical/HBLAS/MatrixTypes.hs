
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP  #-}
{-#  LANGUAGE GADTs,ScopedTypeVariables, PolyKinds,FlexibleInstances,DeriveDataTypeable  #-}






module Numerical.HBLAS.MatrixTypes where

import qualified Data.Vector.Storable as S 
import qualified Data.Vector.Storable.Mutable as SM
import Control.Monad.Primitive  
--import Data.Singletons
import Control.Monad.ST.Safe 
import Data.Typeable 
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

{-
what I really want is this, but its not possible till
datakinds works on types that aren't kind *,


data Eff :: * -> * where
    Pure :: Eff () 
    Mut :: s -> Eff s 
 
data EVector  :: * -> * -> * where 
    PureVector :: S.Vector el  -> EVector Pure el 
    MutVector :: SM.MVector s el -> EVector (Mut s) el  

-}

--data Eff s where
--    Pure :: Eff s 
--    Mut :: s -> Eff s 

--data EVector s el  where 
--    PureVector :: S.Vector el  -> EVector Pure el 
--    MutVector :: SM.MVector s e -> EVector (Mut s) el 



--data Eff = Pure | Mut 

--data EVector  :: Eff -> * -> * -> * where 
--    PureVector :: S.Vector el  -> EVector Pure () el 
--    MutVector :: SM.MVector s el -> EVector Mut s  el      


data Orientation = Row | Column 
    deriving (Eq,Show,Typeable)

data SOrientation :: Orientation -> * where
    SRow :: SOrientation Row 
    SColumn :: SOrientation Column 
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
    deriving (Typeable)
#endif    

instance Show (SOrientation Row) where
     show !a = "SRow"
instance Show (SOrientation Column) where
     show !a = "SColumn"         
instance Eq (SOrientation Row) where
    (==) !a !b = True 
instance Eq (SOrientation Column) where
    (==) !a !b = True 


sTranpose ::  (x~ TransposeF y, y~TransposeF x ) =>SOrientation x -> SOrientation y 
sTranpose SColumn = SRow
sTranpose SRow = SColumn



data Transpose = NoTranspose | Transpose | ConjTranspose | ConjNoTranspose
    deriving(Typeable,Eq,Show)
{-
should think long and hard before adding implicit tranposition to the internal data model
-}

type family TransposeF (x :: Orientation) :: Orientation

type instance TransposeF Row = Column
type instance TransposeF Column = Row 

-- | 'DenseMatrix' is for dense row or column major matrices
data DenseMatrix :: Orientation -> * -> *  where 
    DenseMatrix ::{ _OrientationMat :: SOrientation ornt ,
                    _XdimDenMat :: {-# UNPACK #-}!Int, 
                    _YdimDenMat :: {-# UNPACK #-}!Int ,
                    _StrideDenMat :: {-# UNPACK #-} !Int , 
                    _bufferDenMat :: !(S.Vector elem) }-> DenseMatrix ornt  elem 
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
    deriving (Typeable)
#endif

{-
need to handle rendering a slice differently than a direct matrix 
-}
instance (Show el,SM.Storable el )=> Show (DenseMatrix Row el) where
    show mat@(DenseMatrix SRow xdim ydim stride buffer)
             |  stride == xdim = "DenseMatrix SRow " ++ " " ++show xdim ++ " "  ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
             | otherwise = show $ mapDenseMatrix id mat 

instance (Show el,SM.Storable el )=> Show (DenseMatrix Column el) where
    show mat@(DenseMatrix SColumn xdim ydim stride buffer)
             |  stride == xdim = "DenseMatrix SColumn " ++ " " ++show xdim ++ " " ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
             | otherwise = show $ mapDenseMatrix id mat                         

-- | 'MDenseMatrix' 
data MutDenseMatrix :: *  ->Orientation  -> * -> *  where 
    MutableDenseMatrix :: { _OrientationMutMat :: SOrientation ornt ,
                            _XdimDenMutMat :: {-# UNPACK #-}!Int , 
                            _YdimDenMutMat ::  {-# UNPACK #-}!Int,
                            _StrideDenMutMat :: {-# UNPACK #-} !Int,
                            _bufferDenMutMat :: {-# UNPACK #-} !(SM.MVector s  elem) } -> MutDenseMatrix s ornt elem

--instance (Show el,SM.Storable el, PrimMonad m )=> Show (DenseMatrix (PrimState m) Row el) where
--    show mat@(DenseMatrix SRow xdim ydim stride buffer)
--             |  stride == xdim = "MutableDenseMatrix SRow " ++ " " ++show xdim ++ " "  ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
--             | otherwise = show $ mapDenseMatrix id mat 

--instance (Show el,SM.Storable el,PrimMonad m )=> Show (DenseMatrix (PrimState m ) Column el) where
--    show mat@(DenseMatrix SColumn xdim ydim stride buffer)
--             |  stride == xdim = "DenseMatrix SColumn " ++ " " ++show xdim ++ " " ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
--             | otherwise = show $ mapDenseMatrix id mat    

type IODenseMatrix = MutDenseMatrix RealWorld 
--type MutDenseMatrixIO  or elem  = 

-- data PaddedSymmetricMatrix
-- data PaddedHermetianMatrix
--data PaddedTriangularMatrix 
--- these three may just be wrappers for general dense matrices                       

--data SymmetricMatrix
--data HermitianMatrix -- may just be a wrapper for symmetric?
--data TriangularMatrix
--data BandedMatrix
{-#NOINLINE unsafeFreezeDenseMatrix #-}
unsafeFreezeDenseMatrix :: (SM.Storable elem, PrimMonad m)=> MutDenseMatrix (PrimState m) or elem -> m (DenseMatrix or elem)
unsafeFreezeDenseMatrix (MutableDenseMatrix  ornt a b c mv) = do
        v <- S.unsafeFreeze mv 
        return $! DenseMatrix ornt a b c v                          


{-# NOINLINE unsafeThawDenseMatrix #-}
unsafeThawDenseMatrix :: (SM.Storable elem, PrimMonad m)=> DenseMatrix or elem-> m (MutDenseMatrix (PrimState m) or elem)  
unsafeThawDenseMatrix (DenseMatrix ornt a b c v) = do 
        mv <- S.unsafeThaw v
        return $! MutableDenseMatrix ornt a b c mv 


--freezeDenseMatrix 
 

getDenseMatrixRow :: DenseMatrix or elem -> Int
getDenseMatrixRow (DenseMatrix _ _ ydim _ _)= ydim


getDenseMatrixColumn ::  DenseMatrix or elem -> Int
getDenseMatrixColumn (DenseMatrix _  xdim _ _ _)= xdim


getDenseMatrixLeadingDimStride :: DenseMatrix or elem -> Int 
getDenseMatrixLeadingDimStride (DenseMatrix _  _ _ stride _ ) = stride


getDenseMatrixArray :: DenseMatrix or elem -> S.Vector elem 
getDenseMatrixArray (DenseMatrix _ _ _ _ arr) = arr

getDenseMatrixOrientation :: DenseMatrix or elem -> SOrientation or 
getDenseMatrixOrientation m = _OrientationMat m 



uncheckedDenseMatrixIndex :: (S.Storable elem )=>  DenseMatrix or elem -> (Int,Int) -> elem 
uncheckedDenseMatrixIndex (DenseMatrix SRow _ _ ystride arr) =  \ (x,y)-> arr `S.unsafeIndex` (x + y * ystride)
uncheckedDenseMatrixIndex (DenseMatrix SColumn _ _ xstride arr) = \ (x,y)-> arr `S.unsafeIndex`  (y + x* xstride)

uncheckedDenseMatrixIndexM :: (Monad m ,S.Storable elem )=>  DenseMatrix or elem -> (Int,Int) -> m elem 
uncheckedDenseMatrixIndexM (DenseMatrix SRow _ _ ystride arr) =  \ (x,y)-> return $! arr `S.unsafeIndex` (x + y * ystride)
uncheckedDenseMatrixIndexM (DenseMatrix SColumn _ _ xstride arr) = \ (x,y)-> return $! arr `S.unsafeIndex` (y + x* xstride)

uncheckedMutDenseMatrixIndexM :: (PrimMonad m ,S.Storable elem )=>  MutDenseMatrix (PrimState m) or elem -> (Int,Int) -> m elem 
uncheckedMutDenseMatrixIndexM (MutableDenseMatrix SRow _ _ ystride arr) =  \ (x,y)->  arr `SM.unsafeRead` (x + y * ystride)
uncheckedMutDenseMatrixIndexM (MutableDenseMatrix SColumn _ _ xstride arr) = \ (x,y)->   arr `SM.unsafeRead` (y + x* xstride)

swap :: (a,b)->(b,a)
swap = \ (!x,!y)-> (y,x)
{-# INLINE swap #-}


mapDenseMatrix :: (S.Storable a, S.Storable b) =>  (a->b) -> DenseMatrix or a -> DenseMatrix or b 
mapDenseMatrix f rm@(DenseMatrix SRow xdim ydim _ _) =
    DenseMatrix SRow xdim ydim xdim $!
             S.generate (xdim * ydim) (\ix -> f $! uncheckedDenseMatrixIndex rm (swap $ quotRem ix xdim ) ) 
mapDenseMatrix f rm@(DenseMatrix SColumn xdim ydim _ _) =     
    DenseMatrix SColumn xdim ydim ydim $!
         S.generate (xdim * ydim ) (\ix -> f $! uncheckedDenseMatrixIndex rm ( quotRem ix ydim ) )


imapDenseMatrix :: (S.Storable a, S.Storable b) =>  ((Int,Int)->a->b) -> DenseMatrix or a -> DenseMatrix or b 
imapDenseMatrix f rm@(DenseMatrix sornt xdim ydim _ _) = 
        generateDenseMatrix sornt (xdim,ydim)  (\ix -> f ix  $! uncheckedDenseMatrixIndex rm ix )


-- | In Matrix format memory order enumeration of the index tuples, for good locality 2dim map
uncheckedDenseMatrixNextTuple :: DenseMatrix or elem -> (Int,Int) -> Maybe (Int,Int)
uncheckedDenseMatrixNextTuple (DenseMatrix SRow xdim ydim _ _) = 
        \(!x,!y)-> if  (x >= xdim && y >= ydim) then  Nothing else Just  $! swap $! quotRem (x+ xdim * y + 1) xdim  
uncheckedDenseMatrixNextTuple (DenseMatrix SColumn xdim ydim _ _ ) = 
        \(!x,!y) -> if (x >= xdim && y >=  ydim) then Nothing else Just  $! quotRem (y + ydim * x + 1) ydim 
                                                        --- dont need the swap for column major




generateDenseMatrix :: (S.Storable a)=> SOrientation x -> (Int,Int)->((Int,Int)-> a) -> DenseMatrix x a 
generateDenseMatrix SRow (xdim,ydim) f = DenseMatrix SRow  xdim ydim xdim $!
             S.generate (xdim * ydim) (\ix -> let !ixtup@(!_,!_) = swap $ quotRem ix xdim in 
                                         f  ixtup ) 
generateDenseMatrix SColumn (xdim,ydim) f = DenseMatrix SColumn xdim ydim ydim $!
         S.generate (xdim * ydim ) (\ix -> let  ixtup@(!_,!_) = ( quotRem ix ydim ) in 
                                         f ixtup )    

{-# NOINLINE generateMutableDenseMatrix #-}
generateMutableDenseMatrix :: (S.Storable a,PrimMonad m)=> 
    SOrientation x -> (Int,Int)->((Int,Int)-> a) -> m  (MutDenseMatrix (PrimState m) x a) 
generateMutableDenseMatrix sor  dims fun = do
     x <- unsafeThawDenseMatrix $! generateDenseMatrix sor dims fun 
     return x 


--- this (uncheckedMatrixSlice) will need to have its inlining quality checked


--- | slice over matrix element in the range (inclusive)  [xstart..xend] X [ystart .. yend]
--- call as  @'uncheckedMatrixSlice' matrix (xstart,ystart) (xend,yend) @
uncheckedDenseMatrixSlice :: (S.Storable elem)=>  DenseMatrix or elem -> (Int,Int)-> (Int,Int)-> DenseMatrix or elem 
uncheckedDenseMatrixSlice (DenseMatrix SRow xdim _ ystride arr) (xstart,ystart) (xend,yend) = res
    where   !res = DenseMatrix SRow (xend - xstart + 1) --  X : n - 0 + 1, because zero indexed
                                (yend - ystart+1)   -- Y : m - 0 + 1, because zero indexed
                                (ystride + xstart + (xdim - xend)) -- how much start and end padding per row
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (xstart+ystart*ystride)
            !ixEnd = (xend+yend*ystride)            
uncheckedDenseMatrixSlice (DenseMatrix SColumn _ ydim xstride arr)  (xstart,ystart) (xend,yend) =  res
    where   !res = DenseMatrix SColumn (xend - xstart + 1) 
                                (yend - ystart+1) 
                                (xstride + ystart + (ydim - yend))
                                (S.slice  ixStart (ixEnd - ixStart) arr   )
            !ixStart = (ystart+xstart*xstride)
            !ixEnd = (yend+xend*xstride)

-- | tranposeMatrix does a shallow transpose that swaps the format and the x y params, but changes nothing
-- in the memory layout. 
-- Most applications where transpose is used in a computation need a deep, copying, tranpose operation
transposeDenseMatrix :: (inor ~ (TransposeF outor) ,   outor ~ (TransposeF inor)  ) =>   DenseMatrix inor elem -> DenseMatrix outor elem 
transposeDenseMatrix (DenseMatrix orient x y stride arr)= (DenseMatrix (sTranpose orient) y x stride arr)



{-
need an init with index/ map with index, etc utils≤

-}



