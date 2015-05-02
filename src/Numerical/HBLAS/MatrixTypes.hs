
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP  #-}
{-# LANGUAGE GADTs,ScopedTypeVariables, PolyKinds,FlexibleInstances,DeriveDataTypeable  #-}



{-| PSA, the matrix data types used in the hBLAS binding
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
It is the role of a higher level library to provide any generic operations.

One such higher level lib you can interface with easily is Numerical.
There is a work in progress binding to help this in the numerical-hblas package
(which may not be public yet at the time of this writing)

-}


module Numerical.HBLAS.MatrixTypes where

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Control.Monad.Primitive

import Data.Typeable




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

type Row = 'Row
type Column = 'Column

data SOrientation :: Orientation -> * where
    SRow :: SOrientation Row
    SColumn :: SOrientation Column
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
    deriving (Typeable)
#endif

instance Show (SOrientation Row) where
     show _ = "SRow"
instance Show (SOrientation Column) where
     show _ = "SColumn"
instance Eq (SOrientation Row) where
    (==) _ _ = True
instance Eq (SOrientation Column) where
    (==) _ _ = True


sTranpose ::  (x~ TransposeF y, y~TransposeF x ) =>SOrientation x -> SOrientation y
sTranpose SColumn = SRow
sTranpose SRow = SColumn


--- | May Blas and Lapack routines allow you to implicitly tranpose your argumments
data Transpose = NoTranspose | Transpose | ConjTranspose | ConjNoTranspose
    deriving(Typeable,Eq,Show)

-- | For Symmetric, Hermetian or Triangular matrices, which part is modeled.
---  Applies to both Padded and Packed variants
data MatUpLo = MatUpper | MatLower
    deriving(Typeable,Eq,Show)


-- | Many triangular matrix routines expect to know if the matrix is
-- all 1  (unit ) on the diagonal or not. Likewise, Many Factorizations routines
-- can be assumed to return unit triangular matrices
data MatDiag=  MatUnit | MatNonUnit
    deriving(Typeable,Eq,Show)

-- | For certain Square matrix product, do you want to Compute A*B or B*A
-- only used as an argument
data EquationSide = LeftSide | RightSide
    deriving(Typeable,Eq,Show)
{-
should think long and hard before adding implicit transposition to the internal data model
-}

type family TransposeF (x :: Orientation) :: Orientation

type instance TransposeF Row = Column
type instance TransposeF Column = Row





data Variant = Direct | Implicit
    deriving(Typeable,Eq,Show)
-- | 'Variant' and 'SVariant' are a bit odd looking,
-- They crop up when needing to talk about eg the row vectors of a
-- packed triangular row major matrix wrt both their logical size and manifest sizes
-- this notion only makes sense in the 1dim case.
-- If you don't understand this parameter, just use 'SDirect' and 'Direct'
-- as they will generally be the correct choice for most users.
data SVariant :: Variant -> * where
    SImplicit :: {_frontPadding ::{-UNPACK-} !Int, _endPadding:: {-#UNPACK#-} !Int } -> SVariant Implicit
    SDirect :: SVariant Direct

data DenseVector :: Variant -> * -> * where
    DenseVector :: { _VariantDenseVect ::  !(SVariant varnt)
                    ,_LogicalDimDenseVector :: {-#UNPACK#-}!Int
                    ,_StrideDenseVector :: {-#UNPACK#-} ! Int
                    ,_bufferDenseVector :: !(S.Vector elem)
                      } -> DenseVector varnt elem
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
  deriving (Typeable)
#endif

data MDenseVector :: * -> Variant -> * -> * where
    MutableDenseVector :: { _VariantMutDenseVect ::  !(SVariant varnt)
                        ,_LogicalDimMutDenseVector :: {-#UNPACK#-}!Int
                        ,_StrideMutDenseVector :: {-#UNPACK#-} ! Int
                        ,_bufferMutDenseVector :: !(S.MVector  s elem)
                          } -> MDenseVector s varnt elem
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
    deriving (Typeable)
#endif

-- | 'DenseMatrix' is for dense row or column major matrices
data DenseMatrix :: Orientation -> * -> *  where
    DenseMatrix ::{ _OrientationMat :: SOrientation ornt ,
                    _XdimDenMat :: {-# UNPACK #-}!Int,
                    _YdimDenMat :: {-# UNPACK #-}!Int ,
                    _StrideDenMat :: {-# UNPACK #-} !Int ,
                    _bufferDenMat :: {-#UNPACK#-}!(S.Vector elem) }-> DenseMatrix ornt  elem
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
    deriving (Typeable)
#endif

-- | this should never be used in real code, ever ever, but its handy for testing
-- but seriously never use this in real code, it doesn't do what you think
-- because in the case of a matrix slice, the underlying buffer will have
-- additional elements aside from the ones you expect!
-- never use this in real code please. :)
mutableVectorToList :: (PrimMonad m, S.Storable a) => S.MVector (PrimState m) a -> m [a]
mutableVectorToList mv =  do
        v <- S.unsafeFreeze mv
        return (S.toList v )
{-# NOINLINE mutableVectorToList #-}

{-
need to handle rendering a slice differently than a direct matrix
-}
instance (Show el,SM.Storable el )=> Show (DenseMatrix Row el) where
    show mat@(DenseMatrix SRow xdim ydim stride buffer)
             |  stride == xdim = "DenseMatrix SRow " ++ " " ++show xdim ++ " "  ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
             | otherwise = show $ mapDenseMatrix id mat

instance (Show el,SM.Storable el )=> Show (DenseMatrix Column el) where
    show mat@(DenseMatrix SColumn xdim ydim stride buffer)
             |  stride == ydim = "DenseMatrix SColumn " ++ " " ++show xdim ++ " " ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
             | otherwise = show $ mapDenseMatrix id mat

-- | 'MDenseMatrix'
data MDenseMatrix :: *  ->Orientation  -> * -> *  where
    MutableDenseMatrix :: { _OrientationMutMat :: SOrientation ornt ,
                            _XdimDenMutMat :: {-# UNPACK #-}!Int ,
                            _YdimDenMutMat ::  {-# UNPACK #-}!Int,
                            _StrideDenMutMat :: {-# UNPACK #-} !Int,
                            _bufferDenMutMat :: {-# UNPACK #-} !(SM.MVector s  elem) } -> MDenseMatrix s ornt elem

--instance (Show el,SM.Storable el, PrimMonad m )=> Show (DenseMatrix (PrimState m) Row el) where
--    show mat@(DenseMatrix SRow xdim ydim stride buffer)
--             |  stride == xdim = "MutableDenseMatrix SRow " ++ " " ++show xdim ++ " "  ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
--             | otherwise = show $ mapDenseMatrix id mat

--instance (Show el,SM.Storable el,PrimMonad m )=> Show (DenseMatrix (PrimState m ) Column el) where
--    show mat@(DenseMatrix SColumn xdim ydim stride buffer)
--             |  stride == xdim = "DenseMatrix SColumn " ++ " " ++show xdim ++ " " ++ show ydim ++ " " ++ show stride ++ "(" ++ show buffer ++ ")"
--             | otherwise = show $ mapDenseMatrix id mat

type IODenseMatrix = MDenseMatrix RealWorld
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
unsafeFreezeDenseMatrix :: (SM.Storable elem, PrimMonad m)=> MDenseMatrix (PrimState m) or elem -> m (DenseMatrix or elem)
unsafeFreezeDenseMatrix (MutableDenseMatrix  ornt a b c mv) = do
        v <- S.unsafeFreeze mv
        return $! DenseMatrix ornt a b c v


{-# NOINLINE unsafeThawDenseMatrix #-}
unsafeThawDenseMatrix :: (SM.Storable elem, PrimMonad m)=> DenseMatrix or elem-> m (MDenseMatrix (PrimState m) or elem)
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

uncheckedMutableDenseMatrixIndexM :: (PrimMonad m ,S.Storable elem )=>  MDenseMatrix (PrimState m) or elem -> (Int,Int) -> m elem
uncheckedMutableDenseMatrixIndexM (MutableDenseMatrix SRow _ _ ystride arr) =  \ (x,y)->  arr `SM.unsafeRead` (x + y * ystride)
uncheckedMutableDenseMatrixIndexM (MutableDenseMatrix SColumn _ _ xstride arr) = \ (x,y)->   arr `SM.unsafeRead` (y + x* xstride)

swap :: (a,b)->(b,a)
swap = \ (!x,!y)-> (y,x)
{-# INLINE swap #-}

-- | `map f matrix`
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



-- | generateDenseMatrix Row (k,k) \(i,j)-> if i == j then 1.0 else 0.0 would generate a KxK identity matrix
generateDenseMatrix :: (S.Storable a)=> SOrientation x -> (Int,Int)->((Int,Int)-> a) -> DenseMatrix x a
generateDenseMatrix SRow (xdim,ydim) f = DenseMatrix SRow  xdim ydim xdim $!
             S.generate (xdim * ydim) (\ix -> let !ixtup@(!_,!_) = swap $ quotRem ix xdim in
                                         f  ixtup )
generateDenseMatrix SColumn (xdim,ydim) f = DenseMatrix SColumn xdim ydim ydim $!
         S.generate (xdim * ydim ) (\ix -> let  ixtup@(!_,!_) = ( quotRem ix ydim ) in
                                         f ixtup )




-- | mutable version of generateDenseMatrix
{-# NOINLINE generateMutableDenseMatrix #-}
generateMutableDenseMatrix :: (S.Storable a,PrimMonad m)=>
    SOrientation x -> (Int,Int)->((Int,Int)-> a) -> m  (MDenseMatrix (PrimState m) x a)
generateMutableDenseMatrix sor  dims fun = do
    x <- unsafeThawDenseMatrix $! generateDenseMatrix sor dims fun
    return x

{-# NOINLINE generateMutableUpperTriangular #-}
generateMutableUpperTriangular :: forall a x m . (Num a, S.Storable a, PrimMonad m)=>
    SOrientation x -> (Int,Int)->((Int,Int)-> a) -> m  (MDenseMatrix (PrimState m) x a)
generateMutableUpperTriangular sor dims fun = do
    x <- unsafeThawDenseMatrix $! generateDenseMatrix sor dims trimFun
    return x
        where trimFun (x, y) = (if x>=y then fun (x, y) else (0 :: a))
     
{-# NOINLINE generateMutableLowerTriangular #-}
generateMutableLowerTriangular :: forall a x m . (Num a, S.Storable a,PrimMonad m)=>
    SOrientation x -> (Int,Int)->((Int,Int)-> a) -> m  (MDenseMatrix (PrimState m) x a)
generateMutableLowerTriangular sor dims fun = do
    x <- unsafeThawDenseMatrix $! generateDenseMatrix sor dims trimFun
    return x
        where trimFun (x, y) = (if x<=y then fun (x, y) else (0 :: a))

{-#NOINLINE generateMutableDenseVector#-}
generateMutableDenseVector :: (S.Storable a,PrimMonad m) => Int -> (Int -> a) ->
     m (MDenseVector (PrimState m ) Direct a)
generateMutableDenseVector size init = do
    mv <- S.unsafeThaw $ S.generate size init
    return $! MutableDenseVector SDirect size 1 mv

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



