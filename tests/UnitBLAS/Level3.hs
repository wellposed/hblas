-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module UnitBLAS.Level3(unitTestLevel3BLAS) where

--import Numerical.Array.Shape as S
import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV

import Data.Complex

import  Numerical.HBLAS.MatrixTypes as Matrix
import  Numerical.HBLAS.BLAS.Level3 as BLAS

matmatTest1SGEMM:: IO ()
matmatTest1SGEMM = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const (1.0 :: Float))
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const (1.0 :: Float))
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const (0.0 :: Float))
    BLAS.sgemm Matrix.NoTranspose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2,2,2,2]

matmatTest1DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest1DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (2,2) (const 0.0)
    BLAS.dgemm Matrix.NoTranspose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

matmatTest2DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest2DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (3,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (5,3) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (5,2) (const 0.0)
    BLAS.dgemm Matrix.NoTranspose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 10 3

matmatTest3DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest3DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (2,3) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (5,3) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (5,2) (const 0.0)
    BLAS.dgemm Matrix.Transpose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 10 3

matmatTest3aDGEMM :: Matrix.SOrientation x -> IO ()
matmatTest3aDGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (2,3) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (2,3) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (3,3) (const 0.0)
    BLAS.dgemm Matrix.NoTranspose Matrix.Transpose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 9 2

matmatTest4DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest4DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (3,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (3,5) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (5,2) (const 0.0)
    BLAS.dgemm Matrix.NoTranspose Matrix.Transpose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 10 3

matmatTest5DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest5DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or  (2,3) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or  (3,5) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or  (5,2) (const 0.0)
    BLAS.dgemm Matrix.Transpose Matrix.Transpose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 10 3

matmatTest6DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest6DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (3,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (3,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (3,3) (const 0.0)
    BLAS.dgemm Matrix.Transpose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 9 2

matmatTest7DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest7DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (2,64) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (2,64) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (2,2) (const 0.0)
    BLAS.dgemm Matrix.Transpose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 4 64

matmatTest8DGEMM :: Matrix.SOrientation x -> IO ()
matmatTest8DGEMM or = do
    left  <- Matrix.generateMutableDenseMatrix or (2,9) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix or (2,9) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (2,2) (const 0.0)
    BLAS.dgemm Matrix.Transpose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 4 9

matmatTest1CGEMM:: IO ()
matmatTest1CGEMM = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 0.0)
    BLAS.cgemm Matrix.NoTranspose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

matmatTest1ZGEMM:: IO ()
matmatTest1ZGEMM = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 0.0)
    BLAS.zgemm Matrix.NoTranspose Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

-- [1:+0    1:+1    1:+1]   [1 1]   [3:+2    3:+2   ]
-- [1:+(-1) 1:+0    2:+2] * [1 1] = [4:+1    4:+1   ]
-- [1:+(-1) 2:+(-2) 1:+0]   [1 1]   [4:+(-3) 4:+(-3)]
matmatTest1CHEMM :: IO ()
matmatTest1CHEMM = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (const 0.0)
    BLAS.chemm Matrix.LeftSide Matrix.MatUpper 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [3:+2, 3:+2, 4:+1, 4:+1, 4:+(-3), 4:+(-3)]

-- [1 1 1]   [1:+0 1:+(-1) 1:+(-1)]    [3:+2 4:+1 4:+(-3)]
-- [1 1 1] * [1:+1 1:+0    2:+(-2)]  = [3:+2 4:+1 4:+(-3)]
--           [1:+1 2:+2    1:+0   ]
matmatTest1ZHEMM :: IO ()
matmatTest1ZHEMM = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+1, 1:+0, 0:+0, 1:+1, 2:+2, 1:+0] !! (x + y * 3))
    right <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (const 0.0)
    BLAS.zhemm Matrix.RightSide Matrix.MatLower 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [3:+2, 3:+2, 4:+1, 4:+1, 4:+(-3), 4:+(-3)]

-- [1 2]   [1 3 5]   [1:+0    1:+1    1:+1]   [6:+0 12:+1 18:+1]
-- [3 4] * [2 4 6] + [1:+(-1) 1:+0    2:+2] = [0:+0 26:+0 41:+2]
-- [5 6]             [1:+(-1) 2:+(-2) 1:+0]   [0:+0 0:+0  62:+0]
matmatTest1CHERK :: IO ()
matmatTest1CHERK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    BLAS.cherk Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [6:+0, 12:+1, 18:+1, 0:+0, 26:+0, 41:+2, 0:+0, 0:+0, 62:+0]

-- [1:-1 2]   [1:+1 3 5]   [1:+0 1:+(-1) 1:+(-1)]   [7:+0  0:+0  0:+0]
-- [3    4] * [2    4 6] + [1:+1 1:+0    2:+(-2)] = [12:+4 26:+0 0:+0]
-- [5    6]                [1:+1 2:+2    1:+0   ]   [18:+6 41:+2 62:+0]
matmatTest1ZHERK :: IO ()
matmatTest1ZHERK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+1, 1:+0, 0:+0, 1:+1, 2:+2, 1:+0] !! (x + y * 3))
    BLAS.zherk Matrix.MatLower Matrix.ConjTranspose 1.0 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [7:+0, 12:+4, 18:+6, 0:+0, 26:+0, 41:+2, 0:+0, 0:+0, 62:+0]

-- [1 2]   [1 3 5]       [1:+0    1:+1    1:+1]   [11:+0 23:+1  35:+1]
-- [3 4] * [2 4 6] * 2 + [1:+(-1) 1:+0    2:+2] = [0:+0  51:+0  80:+2]
-- [5 6]                 [1:+(-1) 2:+(-2) 1:+0]   [0:+0  0:+0  123:+0]
matmatTest1CHER2K :: IO ()
matmatTest1CHER2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    BLAS.cher2k Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a b c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [11:+0, 23:+1, 35:+1, 0:+0, 51:+0, 80:+2, 0:+0, 0:+0, 123:+0]

-- [1:-1 2]   [1:+1 3 5]       [1:+0 1:+(-1) 1:+(-1)]   [13:+0  0:+0    0:+0]
-- [3    4] * [2    4 6] * 2 + [1:+1 1:+0    2:+(-2)] = [23:+7  51:+0   0:+0]
-- [5    6]                    [1:+1 2:+2    1:+0   ]   [35:+11 80:+2 123:+0]
matmatTest1ZHER2K :: IO ()
matmatTest1ZHER2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+1, 1:+0, 0:+0, 1:+1, 2:+2, 1:+0] !! (x + y * 3))
    BLAS.zher2k Matrix.MatLower Matrix.ConjTranspose 1.0 1.0 a b c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [13:+0, 23:+7, 35:+11, 0:+0, 51:+0, 80:+2, 0:+0, 0:+0, 123:+0]

matmatTest1SSYMM:: IO ()
matmatTest1SSYMM = do
    left  <- Matrix.generateMutableUpperTriangular (Matrix.SRow)  (2,2) (const (1.0 :: Float))
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const (1.0 :: Float))
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const (0.0 :: Float))
    BLAS.ssymm Matrix.LeftSide Matrix.MatUpper 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2,2,2,2]

matmatTest1DSYMM :: Matrix.SOrientation x -> Matrix.MatUpLo -> IO ()
matmatTest1DSYMM or uplo = do
    left  <- (if uplo == Matrix.MatUpper then Matrix.generateMutableUpperTriangular or (2,2) (const 1.0)
                else Matrix.generateMutableLowerTriangular or (2,2) (const 1.0))
    right <- Matrix.generateMutableDenseMatrix or (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (2,2) (const 0.0)
    BLAS.dsymm Matrix.LeftSide uplo 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

matmatTest2DSYMM :: Matrix.SOrientation x -> Matrix.MatUpLo -> IO ()
matmatTest2DSYMM or uplo = do
    left  <- (if uplo == Matrix.MatUpper then Matrix.generateMutableUpperTriangular or (2,2) (const 1.0)
                else Matrix.generateMutableLowerTriangular or (2,2) (const 1.0))
    right <- Matrix.generateMutableDenseMatrix or (3,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (3,2) (const 0.0)
    BLAS.dsymm Matrix.LeftSide uplo 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0,2.0,2.0]

matmatTest3DSYMM :: Matrix.SOrientation x -> Matrix.MatUpLo -> IO ()
matmatTest3DSYMM or uplo = do
    left  <- (if uplo == Matrix.MatUpper then Matrix.generateMutableUpperTriangular or (2,2) (const 1.0)
                else Matrix.generateMutableLowerTriangular or (2,2) (const 1.0))
    right <- Matrix.generateMutableDenseMatrix or (2,5) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix or (2,5) (const 0.0)
    BLAS.dsymm Matrix.RightSide uplo 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= replicate 10 2

matmatTest1CSYMM:: IO ()
matmatTest1CSYMM = do
    left  <- Matrix.generateMutableUpperTriangular (Matrix.SRow)  (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 0.0)
    BLAS.csymm Matrix.LeftSide Matrix.MatUpper 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

matmatTest1ZSYMM:: IO ()
matmatTest1ZSYMM = do
    left  <- Matrix.generateMutableUpperTriangular (Matrix.SRow)  (2,2) (const 1.0)
    right <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 1.0)
    res   <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (const 0.0)
    BLAS.zsymm Matrix.LeftSide Matrix.MatUpper 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
    resList @?= [2.0,2.0,2.0,2.0]

-- [1 2]   [1 3 5]   [1 1 1]   [6 12 18]
-- [3 4] * [2 4 6] + [1 1 2] = [0 26 41]
-- [5 6]             [1 2 1]   [0 0  62]
matmatTest1SSYRK :: IO ()
matmatTest1SSYRK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1, 1, 1, 0, 1, 2, 0, 0, 1] !! (x + y * 3))
    BLAS.ssyrk Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [6, 12, 18, 0, 26, 41, 0, 0, 62]

-- [1 2]   [1 3 5]   [1 1 1]   [6  0  0 ]
-- [3 4] * [2 4 6] + [1 1 2] = [12 26 0 ]
-- [5 6]             [1 2 1]   [18 41 62]
matmatTest1DSYRK :: IO ()
matmatTest1DSYRK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1, 0, 0, 1, 1, 0, 1, 2, 1] !! (x + y * 3))
    BLAS.dsyrk Matrix.MatLower Matrix.Transpose 1.0 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [6, 12, 18, 0, 26, 41, 0, 0, 62]

-- [1 2]   [1 3 5]   [1:+0 1:+1 1:+1]   [6:+0 12:+1 18:+1]
-- [3 4] * [2 4 6] + [1:+1 1:+0 2:+2] = [0:+0 26:+0 41:+2]
-- [5 6]             [1:+1 2:+2 1:+0]   [0:+0 0:+0  62:+0]
matmatTest1CSYRK :: IO ()
matmatTest1CSYRK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    BLAS.csyrk Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [6:+0, 12:+1, 18:+1, 0:+0, 26:+0, 41:+2, 0:+0, 0:+0, 62:+0]

-- [1:+1 2]   [1:+1 3 5]   [1:+0 1:+1 1:+1]   [6:+0  0:+0  0:+0]
-- [3    4] * [2    4 6] + [1:+1 1:+0 2:+2] = [12:+1 26:+0 0:+0]
-- [5    6]                [1:+1 2:+2 1:+0]   [18:+1 41:+2 62:+0]
matmatTest1ZSYRK :: IO ()
matmatTest1ZSYRK = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+1, 1:+0, 0:+0, 1:+1, 2:+2, 1:+0] !! (x + y * 3))
    BLAS.zsyrk Matrix.MatLower Matrix.Transpose 1.0 1.0 a c -- TODO: Matrix.ConjTranspose is invalid to pass to cblas_zsyrk
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [5:+2, 12:+4, 18:+6, 0:+0, 26:+0, 41:+2, 0:+0, 0:+0, 62:+0]

-- [1 2]   [1 3 5]       [1 1 1]   [11 23  35]
-- [3 4] * [2 4 6] * 2 + [1 1 2] = [0  51  80]
-- [5 6]                 [1 2 1]   [0  0  123]
matmatTest1SSYR2K :: IO ()
matmatTest1SSYR2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1, 1, 1, 0, 1, 2, 0, 0, 1] !! (x + y * 3))
    BLAS.ssyr2k Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a b c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [11, 23, 35, 0, 51, 80, 0, 0, 123]

-- [1 2]   [1 3 5]       [1 1 1]   [11 0  0  ]
-- [3 4] * [2 4 6] * 2 + [1 1 2] = [23 51 0  ]
-- [5 6]                 [1 2 1]   [35 80 123]
matmatTest1DSYR2K :: IO ()
matmatTest1DSYR2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1, 3, 5, 2, 4, 6] !! (x + y * 3))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1, 0, 0, 1, 1, 0, 1, 2, 1] !! (x + y * 3))
    BLAS.dsyr2k Matrix.MatLower Matrix.Transpose 1.0 1.0 a b c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [11, 23, 35, 0, 51, 80, 0, 0, 123]

-- [1 2]   [1 3 5]       [1:+0 1:+1 1:+1]   [11:+0 23:+1 35:+1 ]
-- [3 4] * [2 4 6] * 2 + [1:+1 1:+0 2:+2] = [0:+0  51:+0 80:+2 ]
-- [5 6]                 [1:+1 2:+2 1:+0]   [0:+0  0:+0  123:+0]
matmatTest1CSYR2K :: IO ()
matmatTest1CSYR2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    BLAS.csyr2k Matrix.MatUpper Matrix.NoTranspose 1.0 1.0 a b c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [11:+0, 23:+1, 35:+1, 0:+0, 51:+0, 80:+2, 0:+0, 0:+0, 123:+0]

-- [1:+1 2]   [1:+1 3 5]       [1:+0 1:+1 1:+1]   [9:+4   0:+0  0:+0  ]
-- [3    4] * [2    4 6] * 2 + [1:+1 1:+0 2:+2] = [23:+7  51:+0 0:+0  ]
-- [5    6]                    [1:+1 2:+2 1:+0]   [35:+11 80:+2 123:+0]
matmatTest1ZSYR2K :: IO ()
matmatTest1ZSYR2K = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    b <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3,3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+1, 1:+0, 0:+0, 1:+1, 2:+2, 1:+0] !! (x + y * 3))
    BLAS.zsyr2k Matrix.MatLower Matrix.Transpose 1.0 1.0 a b c -- TODO: Matrix.ConjTranspose is invalid to pass to cblas_zsyr2k
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [9:+4, 23:+7, 35:+11, 0:+0, 51:+0, 80:+2, 0:+0, 0:+0, 123:+0]

-- [1 1 1]   [1 4]   [6 15]
-- [0 1 2] * [2 5] = [8 17]
-- [0 0 1]   [3 6]   [3 6 ]
matmatTest1STRMM :: IO ()
matmatTest1STRMM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1, 1, 1, 0, 1, 2, 0, 0, 1] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 3) (\(x, y) -> [1, 4, 2, 5, 3, 6] !! (x + y * 2))
    BLAS.strmm Matrix.LeftSide Matrix.MatUpper Matrix.NoTranspose Matrix.MatUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [6, 15, 8, 17, 3, 6]

-- [1 2 3]   [2 1 1]   [2 5  11]
-- [4 5 6] * [0 2 2] = [8 14 26]
--           [0 0 2]
matmatTest1DTRMM :: IO ()
matmatTest1DTRMM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [2, 0, 0, 1, 2, 0, 1, 2, 2] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 2) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 3))
    BLAS.dtrmm Matrix.RightSide Matrix.MatLower Matrix.Transpose Matrix.MatNonUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [2, 8, 5, 14, 11, 26]

-- [1:+0 1:+1 1:+1]   [1 2]   [9:+8   12:+10]
-- [0:+0 1:+0 2:+2] * [3 4] = [13:+10 16:+12]
-- [0:+0 0:+0 1:+0]   [5 6]   [5:+0   6:+0  ]
matmatTest1CTRMM :: IO ()
matmatTest1CTRMM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 3) (\(x, y) -> [1, 2, 3, 4, 5, 6] !! (x + y * 2))
    BLAS.ctrmm Matrix.LeftSide Matrix.MatUpper Matrix.NoTranspose Matrix.MatUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [9:+8, 12:+10, 13:+10, 16:+12, 5:+0, 6:+0]

-- [1:+1 3 5]   [1:+0 1:+1 1:+1]   [1:+1 3:+2 11:+8 ]
-- [2    4 6] + [0:+0 1:+0 2:+2] = [2:+0 6:+2 16:+10]
--              [0:+0 0:+0 1:+0]
matmatTest1ZTRMM :: IO ()
matmatTest1ZTRMM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+(-1), 1:+0, 0:+0, 1:+(-1), 2:+(-2), 1:+0] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 2) (\(x, y) -> [1:+1, 3, 5, 2, 4, 6] !! (x + y * 3))
    BLAS.ztrmm Matrix.RightSide Matrix.MatLower Matrix.ConjTranspose Matrix.MatNonUnit 1.0 a c -- TODO: Matrix.ConjTranspose is invalid to pass to cblas_zsyr2k
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [1:+1, 2:+0, 3:+2, 6:+2, 11:+8, 16:+10]

-- [1 1 1]   [1 4]   [6 15]
-- [0 1 2] * [2 5] = [8 17]
-- [0 0 1]   [3 6]   [3 6 ]
matmatTest1STRSM :: IO ()
matmatTest1STRSM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1, 1, 1, 0, 1, 2, 0, 0, 1] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 3) (\(x, y) -> [6, 15, 8, 17, 3, 6] !! (x + y * 2))
    BLAS.strsm Matrix.LeftSide Matrix.MatUpper Matrix.NoTranspose Matrix.MatUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [1, 4, 2, 5, 3, 6]

-- [1 2 3]   [2 1 1]   [2 5  11]
-- [4 5 6] * [0 2 2] = [8 14 26]
--           [0 0 2]
matmatTest1DTRSM :: IO ()
matmatTest1DTRSM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [2, 0, 0, 1, 2, 0, 1, 2, 2] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 2) (\(x, y) -> [2, 5, 11, 8, 14, 26] !! (x + y * 3))
    BLAS.dtrsm Matrix.RightSide Matrix.MatLower Matrix.Transpose Matrix.MatNonUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [1, 4, 2, 5, 3, 6]

-- [1:+0 1:+1 1:+1]   [1 2]   [9:+8   12:+10]
-- [0:+0 1:+0 2:+2] * [3 4] = [13:+10 16:+12]
-- [0:+0 0:+0 1:+0]   [5 6]   [5:+0   6:+0  ]
matmatTest1CTRSM :: IO ()
matmatTest1CTRSM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1:+0, 1:+1, 1:+1, 0:+0, 1:+0, 2:+2, 0:+0, 0:+0, 1:+0] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 3) (\(x, y) -> [9:+8, 12:+10, 13:+10, 16:+12, 5:+0, 6:+0] !! (x + y * 2))
    BLAS.ctrsm Matrix.LeftSide Matrix.MatUpper Matrix.NoTranspose Matrix.MatUnit 1.0 a c
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [1, 2, 3, 4, 5, 6]

-- [1:+1 3 5]   [1:+0 1:+1 1:+1]   [1:+1 3:+2 11:+8 ]
-- [2    4 6] + [0:+0 1:+0 2:+2] = [2:+0 6:+2 16:+10]
--              [0:+0 0:+0 1:+0]
matmatTest1ZTRSM :: IO ()
matmatTest1ZTRSM = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [1:+0, 0:+0, 0:+0, 1:+(-1), 1:+0, 0:+0, 1:+(-1), 2:+(-2), 1:+0] !! (x + y * 3))
    c <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 2) (\(x, y) -> [1:+1, 3:+2, 11:+8, 2:+0, 6:+2, 16:+10] !! (x + y * 3))
    BLAS.ztrsm Matrix.RightSide Matrix.MatLower Matrix.ConjTranspose Matrix.MatNonUnit 1.0 a c -- TODO: Matrix.ConjTranspose is invalid to pass to cblas_zsyr2k
    resList <- Matrix.mutableVectorToList $ _bufferDenMutMat c
    resList @?= [1:+1, 2, 3, 4, 5, 6]

unitTestLevel3BLAS = testGroup "BLAS Level 3 tests " [
    testCase "sgemm on 2x2 all 1s"    matmatTest1SGEMM
    ,testCase "dgemm on 2x2 all 1s" $ matmatTest1DGEMM Matrix.SRow
    ,testCase "dgemm on 3x2 and 5x3 all 1s" $ matmatTest2DGEMM Matrix.SRow
    ,testCase "dgemm on 2x3^T and 5x3 all 1s" $ matmatTest3DGEMM Matrix.SRow
    ,testCase "dgemm on 2x3 and 2x3^T all 1s" $ matmatTest3aDGEMM Matrix.SRow
    ,testCase "dgemm on 3x2 and 3x5^T all 1s" $ matmatTest4DGEMM Matrix.SRow
    ,testCase "dgemm on 2x3^T and 3x5^T all 1s" $ matmatTest5DGEMM Matrix.SRow
    ,testCase "dgemm on 3x2^T and 3x2 all 1s" $ matmatTest6DGEMM Matrix.SRow
    ,testCase "dgemm on 2x64^T and 2x64 all 1s" $ matmatTest7DGEMM Matrix.SRow
    ,testCase "dgemm on 2x9^T and 2x9 all 1s" $ matmatTest8DGEMM Matrix.SRow
    ,testCase "dgemm on 2x2 all 1s (column oriented)" $ matmatTest1DGEMM Matrix.SColumn
    ,testCase "dgemm on 3x2 and 5x3 all 1s (column oriented)" $ matmatTest2DGEMM Matrix.SColumn
    ,testCase "dgemm on 2x3^T and 5x3 all 1s (column oriented)" $ matmatTest3DGEMM Matrix.SColumn
    ,testCase "dgemm on 2x3 and 2x3^T all 1s (column oriented)" $ matmatTest3aDGEMM Matrix.SColumn
    ,testCase "dgemm on 3x2 and 3x5^T all 1s (column oriented)" $ matmatTest4DGEMM Matrix.SColumn
    ,testCase "dgemm on 2x3^T and 3x5^T all 1s (column oriented)" $ matmatTest5DGEMM Matrix.SColumn
    ,testCase "dgemm on 3x2^T and 3x2 all 1s (column oriented)" $ matmatTest6DGEMM Matrix.SColumn
    ,testCase "dgemm on 2x64^T and 2x64 all 1s (column oriented)" $ matmatTest7DGEMM Matrix.SColumn
    ,testCase "dgemm on 2x9^T and 2x9 all 1s (column oriented)" $ matmatTest8DGEMM Matrix.SColumn
    ,testCase "cgemm on 2x2 all 1s" matmatTest1CGEMM
    ,testCase "zgemm on 2x2 all 1s" matmatTest1ZGEMM

    ,testCase "chemm on 3x3 and 2x3 with leftside upper (row oriented)" $ matmatTest1CHEMM
    ,testCase "zhemm on 3x3 and 3x2 with rightside lower (column oriented)" $ matmatTest1ZHEMM

    ,testCase "cherk on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CHERK
    ,testCase "zherk on 3x3 and 3x2 with lower conjtranspose (column oriented)" $ matmatTest1ZHERK

    ,testCase "cher2k on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CHER2K
    ,testCase "zher2k on 3x3 and 3x2 with lower conjtranspose (column oriented)" $ matmatTest1ZHER2K

    ,testCase "ssymm on 2x2 upper all 1s" matmatTest1SSYMM
    ,testCase "dsymm on 2x2 upper all 1s" $ matmatTest1DSYMM Matrix.SRow Matrix.MatUpper
    ,testCase "dsymm on 2x2 and 3x2 upper all 1s" $ matmatTest2DSYMM Matrix.SRow Matrix.MatUpper
    ,testCase "dsymm on 2x5 and 2x2 upper all 1s" $ matmatTest3DSYMM Matrix.SRow Matrix.MatUpper
    ,testCase "dsymm on 2x2 lower all 1s" $ matmatTest1DSYMM Matrix.SRow Matrix.MatLower
    ,testCase "dsymm on 2x2 and 3x2 lower all 1s" $ matmatTest2DSYMM Matrix.SRow Matrix.MatLower
    ,testCase "dsymm on 2x5 and 2x2 lower all 1s" $ matmatTest3DSYMM Matrix.SRow Matrix.MatLower
    ,testCase "dsymm on 2x2 upper all 1s (column oriented)" $ matmatTest1DSYMM Matrix.SColumn Matrix.MatUpper
    ,testCase "dsymm on 2x2 and 3x2 upper all 1s (column oriented)" $ matmatTest2DSYMM Matrix.SColumn Matrix.MatUpper
    ,testCase "dsymm on 2x5 and 2x2 upper all 1s (column oriented)" $ matmatTest3DSYMM Matrix.SColumn Matrix.MatUpper
    ,testCase "dsymm on 2x2 lower all 1s (column oriented)" $ matmatTest1DSYMM Matrix.SColumn Matrix.MatLower
    ,testCase "dsymm on 2x2 and 3x2 lower all 1s (column oriented)" $ matmatTest2DSYMM Matrix.SColumn Matrix.MatLower
    ,testCase "dsymm on 2x5 and 2x2 lower all 1s (column oriented)" $ matmatTest3DSYMM Matrix.SColumn Matrix.MatLower
    ,testCase "csymm on 2x2 all 1s" matmatTest1CSYMM
    ,testCase "zsymm on 2x2 all 1s" matmatTest1ZSYMM

    ,testCase "ssyrk on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1SSYRK
    ,testCase "dsyrk on 3x3 and 3x2 with lower transpose (column oriented)" $ matmatTest1DSYRK
    ,testCase "csyrk on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CSYRK
    ,testCase "zsyrk on 3x3 and 3x2 with lower transpose (column oriented)" $ matmatTest1ZSYRK

    ,testCase "ssyr2k on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1SSYR2K
    ,testCase "dsyr2k on 3x3 and 3x2 with lower transpose (column oriented)" $ matmatTest1DSYR2K
    ,testCase "csyr2k on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CSYR2K
    ,testCase "zsyr2k on 3x3 and 3x2 with lower conjtranspose (column oriented)" $ matmatTest1ZSYR2K

    ,testCase "strmm on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1STRMM
    ,testCase "dtrmm on 3x3 and 3x2 with lower transpose (column oriented)" $ matmatTest1DTRMM
    ,testCase "ctrmm on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CTRMM
    ,testCase "ztrmm on 3x3 and 3x2 with lower conjtranspose (column oriented)" $ matmatTest1ZTRMM

    ,testCase "strsm on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1STRSM
    ,testCase "dtrsm on 3x3 and 3x2 with lower transpose (column oriented)" $ matmatTest1DTRSM
    ,testCase "ctrsm on 3x3 and 2x3 with upper no transpose (row oriented)" $ matmatTest1CTRSM
    ,testCase "ztrsm on 3x3 and 3x2 with lower conjtranspose (column oriented)" $ matmatTest1ZTRSM
    ]

----unitTestShape = testGroup "Shape Unit tests"
----    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )
----    , testCase "foldr on shape" $ ( S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldr  (+) 0  [1,2,3])  )
----    , testCase "scanr1 on shape" (S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) )
----    , testCase "scanl1 on shape" (S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) )
----    ]

--turn the following into the first level 2 test

---- {-# LANGUAGE ScopedTypeVariables, DataKinds, CPP #- }
--import Numerical.HBLAS.BLAS.FFI
--import Numerical.HBLAS.BLAS
--import Numerical.HBLAS.MatrixTypes
--import Data.Vector.Storable.Mutable as M
--import qualified Data.Vector.Storable as S

--main :: IO ()
--main = do
--  -- Just test that the symbol resolves
--  --openblas_set_num_threads_unsafe 7
--  v  :: IOVector Double <- M.replicate 10 1.0
-- #if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ <= 704)
--  --this makes 7.4 panic!
--  (leftMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (const 1.0 ::Float )
--  (rightMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (const 1.0 ::Float )
--  (resMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (const 1.0 ::Float )
--  sgemm NoTranspose NoTranspose 1.0 1.0 leftMat rightMat resMat
--  --(MutableDenseMatrix _ _ _ _ buff) <- return resMat
--  theRestMat <- unsafeFreezeDenseMatrix resMat
--  putStrLn $ show theRestMat
-- #endif
----DenseMatrix SRow  5 5 5(fromList [11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,251.0,251.0,251.0,251.0,251.0])
---- THAT IS WRONG
----Need to figure what whats going on here

--  putStrLn $ show res
--  putStrLn "it works!"



