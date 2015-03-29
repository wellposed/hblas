module SymmExample where

import Foreign.Storable
import Numerical.HBLAS.BLAS
import Numerical.HBLAS.MatrixTypes

-- Generate the constant mutable square matrix of the given type and dimensions.
constMatrix :: Storable a
            => Int
            -> a
            -> IO (IODenseMatrix Row a)
constMatrix n k = generateMutableDenseMatrix SRow (n,n) (const k)

example_ssymm :: IO ()
example_ssymm = do
    left  <- constMatrix 2 (2 :: Float)
    right <- constMatrix 2 (3 :: Float)
    out   <- constMatrix 2 (0 :: Float)

    ssymm LeftSide MatUpper 1.0 1.0 left right out

    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting

example_dsymm :: IO ()
example_dsymm = do
    left  <- constMatrix 2 (2 :: Double)
    right <- constMatrix 2 (3 :: Double)
    out   <- constMatrix 2 (0 :: Double)

    dsymm LeftSide MatUpper 1.0 1.0 left right out

    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting

example_csymm :: IO ()
example_csymm = do
    left  <- generateMutableDenseMatrix (SRow)  (2,2) (const 1.0)
    right <- generateMutableDenseMatrix (SRow)  (2,2) (const 1.0)
    out   <- generateMutableDenseMatrix (SRow)  (2,2) (const 0.0)
{-
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 0.0)
-}
    csymm LeftSide MatUpper 1.0 1.0 left right out
    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting

example_zsymm:: IO ()
example_zsymm = do
    left  <- generateMutableDenseMatrix (SRow)  (2,2) (const 1.0)
    right <- generateMutableDenseMatrix (SRow)  (2,2) (const 1.0)
    out   <- generateMutableDenseMatrix (SRow)  (2,2) (const 0.0)
    zsymm LeftSide MatUpper 1.0 1.0 left right out
    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting

main :: IO ()
main = do
  example_ssymm
  example_dsymm
  example_csymm
  example_zsymm
