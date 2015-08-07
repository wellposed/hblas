-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module UnitBLAS.Level2(unitTestLevel2BLAS) where

--import Numerical.Array.Shape as S
import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit


import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV

import Data.Complex

import  Numerical.HBLAS.MatrixTypes as Matrix
import  Numerical.HBLAS.BLAS as BLAS


--unitTestShape = testGroup "Shape Unit tests"
--    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )
--    , testCase "foldr on shape" $ ( S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldr  (+) 0  [1,2,3])  )
--    , testCase "scanr1 on shape" (S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) )
--    , testCase "scanl1 on shape" (S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) )
--    ]

matvecTest1SGBMV :: IO ()
matvecTest1SGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 5) (\_ -> (1.0))
    x <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    BLAS.sgbmv Matrix.NoTranspose 5 5 1 1 1.0 a x 1 0.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2, 3, 3, 3, 2]

matvecTest1DGBMV :: IO ()
matvecTest1DGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 5) (\_ -> (1.0))
    x <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    res <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0))
    BLAS.dgbmv Matrix.NoTranspose 10 5 1 1 1.0 a x 1 1.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [3, 4, 4, 4, 3, 1, 1, 1, 1, 1]
    -- not [3, 4, 4, 4, 3, 2, 1, 1, 1, 1]

matvecTest1CGBMV :: IO ()
matvecTest1CGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 4) (\(x, y) -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 0] !! (x * 4 + y))
    x <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.NoTranspose 4 4 0 1 1.0 a x 1 0.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [0:+4.0, 0:+4.0, 0:+4.0, 0:+2.0]

matvecTest2CGBMV :: IO ()
matvecTest2CGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 4) (\(x, y) -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 0] !! (x * 4 + y))
    x <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.ConjNoTranspose 4 4 0 1 1.0 a x 1 0.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [4.0:+0, 4.0:+0, 4.0:+0, 2.0:+0]

matvecTest1ZGBMV :: IO ()
matvecTest1ZGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 10) (\_ -> (1.0:+1.0))
    x <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.Transpose 10 5 1 0 2.0 a x 1 0.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0]
    -- not [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+4.0]

matvecTest2ZGBMV :: IO ()
matvecTest2ZGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 10) (\_ -> (1.0:+1.0))
    x <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.ConjTranspose 10 5 1 0 2.0 a x 1 0.0 res 1
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [8.0:+0, 8.0:+0, 8.0:+0, 8.0:+0, 8.0:+0]
    -- not [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+4.0]

-- notes for gbmv
-- column > rows: the tail of the supper diagonals is considered.
-- rows > column: the tail of the sub diagonals is not considered.

matmatTest1SGEMV:: IO ()
matmatTest1SGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0::Float))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 :: Float))
    res  <- Matrix.generateMutableDenseVector  2 (\_ -> (0.0 :: Float))
    BLAS.sgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2,2]

matmatTest1DGEMV:: IO ()
matmatTest1DGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector 2  (\_ -> (0.0 ))
    BLAS.dgemv Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2.0,2.0]

matmatTest1CGEMV:: IO ()
matmatTest1CGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector  2 (\_ -> (0.0 ))
    BLAS.cgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2.0,2.0]

matmatTest1ZGEMV:: IO ()
matmatTest1ZGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector 2 (\_ -> (0.0 ))
    BLAS.zgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2.0,2.0]

----
----

matmatTest1SGER :: IO ()
matmatTest1SGER = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,2) (\_ -> 1.0)
  x <- Matrix.generateMutableDenseVector 2 (\_ -> 2.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0)
  BLAS.sger 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [13.0,13.0,13.0,13.0]

matmatTest1DGER :: IO ()
matmatTest1DGER = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,2) (\_ -> 1.0)
  x <- Matrix.generateMutableDenseVector 2 (\_ -> 2.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0)
  BLAS.sger 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [13.0,13.0,13.0,13.0]

matmatTest1CGERC :: IO ()
matmatTest1CGERC = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+3.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+2.0)
  BLAS.cgerc 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [25.0:+11.0, 25.0:+11.0, 1.0:+1.0, 25.0:+11.0, 25.0:+11.0, 1.0:+1.0]
  -- why the following is not correct...
  -- resList @?= [25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0]

matmatTest1ZGERC :: IO ()
matmatTest1ZGERC = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+3.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+2.0)
  BLAS.zgerc 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [25.0:+11.0, 25.0:+11.0, 1.0:+1.0, 25.0:+11.0, 25.0:+11.0, 1.0:+1.0]
  -- why the following is not correct...
  -- resList @?= [25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0]

matmatTest1CGERU :: IO ()
matmatTest1CGERU = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+(-3.0))
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+(-2.0))
  BLAS.cgeru 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0, 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0]
  -- why the following is not correct...
  -- resList @?= [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0)]

matmatTest1ZGERU :: IO ()
matmatTest1ZGERU = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+(-3.0))
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+(-2.0))
  BLAS.zgeru 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList @?= [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0, 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0]
  -- why the following is not correct...
  -- resList @?= [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0)]

-- [1:+0    1:+1    1:+1    0:+0]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [0:+0    1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]
matvectTest1CHBMV :: IO ()
matvectTest1CHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chbmv Matrix.MatUpper 2 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

matvectTest2CHBMV :: IO ()
matvectTest2CHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chbmv Matrix.MatUpper 2 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

-- [1:+0    1:+1    1:+1    0:+0]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [0:+0    1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]
matvectTest1ZHBMV :: IO ()
matvectTest1ZHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 4) (\_ -> 1.0:+(-1.0))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.zhbmv Matrix.MatLower 2 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

-- [1:+0    1:+1    1:+1    1:+1]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [1:+(-1) 1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]
matvectTest1CHEMV :: IO ()
matvectTest1CHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chemv Matrix.MatUpper 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

matvectTest2CHEMV :: IO ()
matvectTest2CHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chemv Matrix.MatUpper 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

matvectTest1ZHEMV :: IO ()
matvectTest1ZHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+(-1.0))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.zhemv Matrix.MatLower 1.0 a x 1 0.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

matvectTest1CHER :: IO ()
matvectTest1CHER = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.cher Matrix.MatUpper 1.0 x 1 a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList @?= [3.0:+0.0, 5.0:+1.0,  7.0:+1.0,  9.0:+1.0,
               1.0:+1.0, 9.0:+0.0, 13.0:+1.0, 17.0:+1.0,
               1.0:+1.0, 1.0:+1.0, 19.0:+0.0, 25.0:+1.0,
               1.0:+1.0, 1.0:+1.0,  1.0:+1.0, 33.0:+0.0]

matvectTest1ZHER :: IO ()
matvectTest1ZHER = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zher Matrix.MatLower 1.0 x 1 a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList @?= [3.0:+0.0,  1.0:+1.0,  1.0:+1.0,  1.0:+1.0,
               5.0:+1.0,  9.0:+0.0,  1.0:+1.0,  1.0:+1.0,
               7.0:+1.0, 13.0:+1.0, 19.0:+0.0,  1.0:+1.0,
               9.0:+1.0, 17.0:+1.0, 25.0:+1.0, 33.0:+0.0]

matvectTest1CHER2 :: IO ()
matvectTest1CHER2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.cher2 Matrix.MatUpper 1.0 x 1 y 1 a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList @?= [5.0:+0.0,  9.0:+1.0, 13.0:+1.0, 17.0:+1.0,
               1.0:+1.0, 17.0:+0.0, 25.0:+1.0, 33.0:+1.0,
               1.0:+1.0,  1.0:+1.0, 37.0:+0.0, 49.0:+1.0,
               1.0:+1.0,  1.0:+1.0,  1.0:+1.0, 65.0:+0.0]

matvectTest1ZHER2 :: IO ()
matvectTest1ZHER2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zher2 Matrix.MatLower 1.0 x 1 y 1 a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList @?= [ 5.0:+0.0,  1.0:+1.0,  1.0:+1.0,  1.0:+1.0,
                9.0:+1.0, 17.0:+0.0,  1.0:+1.0,  1.0:+1.0,
               13.0:+1.0, 25.0:+1.0, 37.0:+0.0,  1.0:+1.0,
               17.0:+1.0, 33.0:+1.0, 49.0:+1.0, 65.0:+0.0]

-- [0:+0    1:+1    2:+2    3:+3]
-- [1:+(-1) 4:+0    5:+5    6:+6]
-- [2:+(-2) 5:+(-5) 7:+0    8:+8]
-- [3:+(-3) 6:+(-6) 8:+(-8) 9:+0]
--
-- [2:+2]
-- [2:+2]
-- [2:+2]
-- [2:+2]
--
-- [ 3:+27]
-- [15:+55]
-- [45:+49]
-- [89:+21]
matvectTest1CHPMV :: IO ()
matvectTest1CHPMV = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [2.0:+2.0, 2.0:+2.0, 2.0:+2.0, 2.0:+2.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [3.0:+3.0, 3.0:+3.0, 3.0:+3.0, 3.0:+3.0] !! idx)
  BLAS.chpmv Matrix.SRow Matrix.MatUpper 4 1.0 a x 1 1.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [ 3.0:+27.0, 15.0:+55.0, 45.0:+49.0, 89.0:+21.0]

-- [0:+0    1:+1    3:+3    6:+6]
-- [1:+(-1) 2:+0    4:+4    7:+7]
-- [3:+(-3) 4:+(-4) 5:+0    8:+8]
-- [6:+(-6) 7:+(-7) 8:+(-8) 9:+0]
--
-- [2:+2]
-- [2:+2]
-- [2:+2]
-- [2:+2]
--
-- [  6:+46]
-- [ 14:+54]
-- [ 44:+48]
-- [108:+24]
matvectTest1ZHPMV :: IO ()
matvectTest1ZHPMV = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [2.0:+2.0, 2.0:+2.0, 2.0:+2.0, 2.0:+2.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [3.0:+3.0, 3.0:+3.0, 3.0:+3.0, 3.0:+3.0] !! idx)
  BLAS.zhpmv Matrix.SColumn Matrix.MatUpper 4 1.0 a x 1 2.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [ 6.0:+46.0, 14.0:+54.0, 44.0:+48.0, 108.0:+24.0]

-- [1:+1]
-- [2:+2]
-- [3:+3]
-- [4:+4]
--
-- [2:+0    4:+0    6:+0    8:+0 ]
-- [4:+0    8:+0    12:+0   16:+0]
-- [6:+0    12:+0   18:+0   24:+0]
-- [8:+0    16:+0   24:+0   32:+0]
--
-- [0:+0    1:+1    3:+3    6:+6]
-- [1:+(-1) 2:+0    4:+4    7:+7]
-- [3:+(-3) 4:+(-4) 5:+0    8:+8]
-- [6:+(-6) 7:+(-7) 8:+(-8) 9:+0]
--
-- [2:+0     5:+1     9:+3     14:+6]
-- [5:+(-1)  10:+0    16:+4    23:+7]
-- [9:+(-3)  16:+(-4) 23:+0    32:+8]
-- [14:+(-6) 23:+(-7) 32:+(-8) 41:+0]
matvectTest1CHPR :: IO ()
matvectTest1CHPR = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.chpr Matrix.SColumn Matrix.MatUpper 4 1.0 x 1 a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList @?= [2.0:+0.0, 5.0:+1.0, 10.0:+0.0, 9.0:+3.0, 16.0:+4.0, 23.0:+0.0, 14.0:+6.0, 23.0:+7.0,  32.0:+8.0, 41.0:+0.0]

-- [1:+1]
-- [2:+2]
-- [3:+3]
-- [4:+4]
--
-- [2:+0    4:+0    6:+0    8:+0 ]
-- [4:+0    8:+0    12:+0   16:+0]
-- [6:+0    12:+0   18:+0   24:+0]
-- [8:+0    16:+0   24:+0   32:+0]
--
-- [0:+0    1:+1    3:+3    6:+6]
-- [1:+(-1) 2:+0    4:+4    7:+7]
-- [3:+(-3) 4:+(-4) 5:+0    8:+8]
-- [6:+(-6) 7:+(-7) 8:+(-8) 9:+0]
--
-- [2:+0     5:+1     9:+3     14:+6]
-- [5:+(-1)  10:+0    16:+4    23:+7]
-- [9:+(-3)  16:+(-4) 23:+0    32:+8]
-- [14:+(-6) 23:+(-7) 32:+(-8) 41:+0]
matvectTest1ZHPR :: IO ()
matvectTest1ZHPR = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+(-0.0), 1.0:+(-1.0), 2.0:+(-2.0), 3.0:+(-3.0), 4.0:+(-4.0), 5.0:+(-5.0), 6.0:+(-6.0), 7.0:+(-7.0), 8.0:+(-8.0), 9.0:+(-9.0)] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zhpr Matrix.SRow Matrix.MatLower 4 1.0 x 1 a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList @?= [2.0:+0.0, 5.0:+(-1.0), 10.0:+0.0, 9.0:+(-3.0), 16.0:+(-4.0), 23.0:+0.0, 14.0:+(-6.0), 23.0:+(-7.0), 32.0:+(-8.0), 41.0:+0.0]

-- [12:+0 ...]
-- [.       ...]
-- [.       ...]
-- [.       ...]
--
matvectTest1CHPR2 :: IO ()
matvectTest1CHPR2 = do
  a <- Matrix.generateMutableDenseVector 10 (\_ -> 0.0:+0.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+2.0, 1.0:+2.0, 1.0:+2.0, 1.0:+2.0] !! idx)
  BLAS.chpr2 Matrix.SColumn Matrix.MatUpper 4 2.0 x 1 y 1 a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList @?= [12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0]

matvectTest1ZHPR2 :: IO ()
matvectTest1ZHPR2 = do
  a <- Matrix.generateMutableDenseVector 10 (\_ -> 0.0:+0.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+2.0, 1.0:+2.0, 1.0:+2.0, 1.0:+2.0] !! idx)
  BLAS.zhpr2 Matrix.SColumn Matrix.MatUpper 4 2.0 x 1 y 1 a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList @?= [12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0]

-- [1 2 0 0]
-- [2 3 4 0]
-- [0 4 5 6]
-- [0 0 6 7]
--
-- [0 2 4 6]
-- [1 3 5 7]
--
-- [5]
-- [11]
-- [17]
-- [15]

-- The storage of a is different from the refrence. In ref it should be [0 2 4 6 1 3 5 7]

matvectTest1SSBMV :: IO ()
matvectTest1SSBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 2) (\(x, y) -> [1, 2, 3, 4, 5, 6, 7, 0] !! (y * 4 + x))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 2)
  BLAS.ssbmv Matrix.MatUpper 1 1.0 a x 1 1.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [5, 11, 17, 15]

matvectTest1DSBMV :: IO ()
matvectTest1DSBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (4, 2) (\(x, y) -> [1, 3, 5, 7, 2, 4, 6, 0] !! (y * 4 + x))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 2)
  BLAS.dsbmv Matrix.MatLower 1 1.0 a x 1 1.0 y 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList @?= [5, 11, 17, 15]

matmatTest1STRSV:: IO ()
matmatTest1STRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
            (\(i,j) -> if i >= j then (1.0::Float) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.strsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2,1]

matmatTest1DTRSV:: IO ()
matmatTest1DTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::Double) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.dtrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2,1]

matmatTest1CTRSV:: IO ()
matmatTest1CTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Float)) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.ctrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2,1]

matmatTest1ZTRSV:: IO ()
matmatTest1ZTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Double )) else 0 )
    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.ztrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList @?= [2,1]

unitTestLevel2BLAS = testGroup "BLAS Level 2 tests " [
----- gbmv tests
    testCase "sgbmv on 3x5 a(5x5 matrix) all 1s" matvecTest1SGBMV
    ,testCase "dgbmv on 3x5 a(10x5 matrix) all 1s with beta 1.0" matvecTest1DGBMV
    ,testCase "cgbmv on 2x4 a(4x4 matrix) all 1+i s" matvecTest1CGBMV
    -- ,testCase "cgbmv on 2x4 a(4x4 matrix) all 1+i s with conjnotranspose" matvecTest2CGBMV  conjnotranspose is invalid...
    ,testCase "zgbmv on 2x10 a(5x10 matrix) all 1+i s with transpose and alpha 1.0" matvecTest1ZGBMV
    ,testCase "zgbmv on 2x10 a(5x10 matrix) all 1+i s with conjtranspose and alpha 1.0" matvecTest2ZGBMV
----- gemv tests
    ,testCase "sgemv on 2x2 all 1s"    matmatTest1SGEMV
    ,testCase "dgemv  on 2x2 all 1s " matmatTest1DGEMV
    ,testCase "cgemv  on 2x2 all 1s" matmatTest1CGEMV
    ,testCase "zgemv on 2x2 all 1s" matmatTest1ZGEMV
---- ger tests
    ,testCase "sger on 2x2 all 1s" matmatTest1SGER
    ,testCase "dger on 2x2 all 1s" matmatTest1DGER
    ,testCase "cgerc on 2x3 all 1+i s" matmatTest1CGERC
    ,testCase "zgerc on 2x3 all 1+i s" matmatTest1ZGERC
    ,testCase "cgeru on 2x3 all 1+i s" matmatTest1CGERU
    ,testCase "zgeru on 2x3 all 1+i s" matmatTest1ZGERU
---- hbmv tests
    ,testCase "chbmv on 4*3 a(4x4 matrix) upper all 1+i s" matvectTest1CHBMV
    ,testCase "chbmv on 4*3 a(4x4 matrix) upper all 1+i s (column oriented)" matvectTest2CHBMV
    ,testCase "zhbmv on 4*3 a(4x4 matrix) lower all 1+i s" matvectTest1ZHBMV
---- hemv tests
    ,testCase "chemv on 4*3 a(4x4 matrix) upper all 1+i s" matvectTest1CHEMV
    ,testCase "chemv on 4*3 a(4x4 matrix) upper all 1+i s (column oriented)" matvectTest2CHEMV
    ,testCase "zhemv on 4*3 a(4x4 matrix) lower all 1+i s" matvectTest1ZHEMV
---- her tests
    ,testCase "cher on 4*4 a upper all 1+i s" matvectTest1CHER
    ,testCase "zher on 4*4 a upper all 1+i s" matvectTest1ZHER
---- her2 tests
    ,testCase "cher2 on 4*4 a upper all 1+i s" matvectTest1CHER2
    ,testCase "zher2 on 4*4 a upper all 1+i s" matvectTest1ZHER2
---- hpmv tests
    ,testCase "chpmv on 4*4 a upper (row oriented)" matvectTest1CHPMV
    ,testCase "zhpmv on 4*4 a upper (column oriented)" matvectTest1ZHPMV
---- hpr tests
    ,testCase "chpr on 4*4 a upper (column oriented)" matvectTest1CHPR
    ,testCase "zhpr on 4*4 a lower (row oriented)" matvectTest1ZHPR
---- hpr2 tests
    ,testCase "chpr2 on 4*4 a upper (column oriented)" matvectTest1CHPR2
    ,testCase "zhpr2 on 4*4 a upper (row oriented)" matvectTest1ZHPR2
---- sbmv tests
    ,testCase "ssbmv on 4*4 a upper (row oriented)" matvectTest1SSBMV
    ,testCase "dsbmv on 4*4 a upper (column oriented)" matvectTest1DSBMV
----- trsv tests
    ,testCase "strsv on 2x2 upper 1s" matmatTest1STRSV
    ,testCase "dtrsv on 2x2 upper 1s" matmatTest1DTRSV
    ,testCase "ctrsv on 2x2 upper 1s" matmatTest1CTRSV
    ,testCase "ztrsv on 2x2 upper 1s" matmatTest1ZTRSV
    ]

