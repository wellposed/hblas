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
    ,testCase "chbmv on 4*3 a(4x4 matrix) all 1+i s" matvectTest1CHBMV
    ,testCase "chbmv on 4*3 a(4x4 matrix) all 1+i s (column oriented)" matvectTest2CHBMV
    ,testCase "zhbmv on 4*3 a(4x4 matrix) all 1+i s" matvectTest1ZHBMV
----- trsv tests
    ,testCase "strsv on 2x2 upper 1s" matmatTest1STRSV
    ,testCase "dtrsv on 2x2 upper 1s" matmatTest1DTRSV
    ,testCase "ctrsv on 2x2 upper 1s" matmatTest1CTRSV
    ,testCase "ztrsv on 2x2 upper 1s" matmatTest1ZTRSV
    ]

