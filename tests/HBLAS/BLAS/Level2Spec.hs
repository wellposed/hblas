-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module HBLAS.BLAS.Level2Spec(main, spec) where

import Data.Complex

import Test.Hspec

import Numerical.HBLAS.MatrixTypes as Matrix
import Numerical.HBLAS.BLAS.Level2 as BLAS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  gbmvSpec
  gemvSpec
  gerSpec
  gercSpec
  geruSpec
  hbmvSpec
  hemvSpec
  herSpec
  her2Spec
  hpmvSpec
  hprSpec
  hpr2Spec
  sbmvSpec
  spmvSpec
  sprSpec
  spr2Spec
  symvSpec
  syrSpec
  syr2Spec
  tbmvSpec
  tbsvSpec
  tpmvSpec
  tpsvSpec
  trmvSpec
  trsvSpec
  
gbmvSpec :: Spec
gbmvSpec =
  context "?GBMV" $ do
    describe "SGBMV" $ do
      it "3x5 a(5x5 matrix) all 1's" $ do
        matvecTest1SGBMV
    describe "DBGMV" $ do
      it "3x5 a(10x5 matrix) all 1's with beta 1.0" $ do
        matvecTest1DGBMV
    describe "CGBMV" $ do
      it "2x4 a(4x4 matrix) all 1+i's" $ do
        matvecTest1CGBMV
     -- it "gbmv on 2x4 a(4x4 matrix) all 1+i s with conjnotranspose" $ do
     --   matvecTest2CGBMV -- conjnotranspose is invalid
    describe "ZGBMV" $ do
      it "2x10 a(5x10 matrix) all 1+i s with transpose and alpha 1.0" $ do
        matvecTest1ZGBMV
      it "2x10 a(5x10 matrix) all 1+i s with conjtranspose and alpha 1.0" $ do
        matvecTest2ZGBMV
        

matvecTest1SGBMV :: IO ()
matvecTest1SGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 5) (\_ -> (1.0))
    x <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    BLAS.sgbmv Matrix.NoTranspose 5 5 1 1 1.0 a x 0.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2, 3, 3, 3, 2]

matvecTest1DGBMV :: IO ()
matvecTest1DGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 5) (\_ -> (1.0))
    x <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0))
    res <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0))
    BLAS.dgbmv Matrix.NoTranspose 10 5 1 1 1.0 a x 1.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [3, 4, 4, 4, 3, 1, 1, 1, 1, 1]
    -- not [3, 4, 4, 4, 3, 2, 1, 1, 1, 1]

matvecTest1CGBMV :: IO ()
matvecTest1CGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 4) (\(x, y) -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 0] !! (x * 4 + y))
    x <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.NoTranspose 4 4 0 1 1.0 a x 0.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [0:+4.0, 0:+4.0, 0:+4.0, 0:+2.0]

matvecTest2CGBMV :: IO ()
matvecTest2CGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 4) (\(x, y) -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 0] !! (x * 4 + y))
    x <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 4 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.ConjNoTranspose 4 4 0 1 1.0 a x 0.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [4.0:+0, 4.0:+0, 4.0:+0, 2.0:+0]

matvecTest1ZGBMV :: IO ()
matvecTest1ZGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 10) (\_ -> (1.0:+1.0))
    x <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.Transpose 10 5 1 0 2.0 a x 0.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0]
    -- not [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+4.0]

matvecTest2ZGBMV :: IO ()
matvecTest2ZGBMV = do
    a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2, 10) (\_ -> (1.0:+1.0))
    x <- Matrix.generateMutableDenseVector 10 (\_ -> (1.0:+1.0))
    res <- Matrix.generateMutableDenseVector 5 (\_ -> (1.0:+1.0))
    BLAS.cgbmv Matrix.ConjTranspose 10 5 1 0 2.0 a x 0.0 res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [8.0:+0, 8.0:+0, 8.0:+0, 8.0:+0, 8.0:+0]
    -- not [0:+8.0, 0:+8.0, 0:+8.0, 0:+8.0, 0:+4.0]

-- notes for gbmv
-- column > rows: the tail of the supper diagonals is considered.
-- rows > column: the tail of the sub diagonals is not considered.


gemvSpec :: Spec
gemvSpec =
  context "?GEMV" $ do
    describe "SGEMV" $ do
      it "2x2 all 1's" $ do
        matmatTest1SGEMV
    describe "DGEMV" $ do
      it "2x2 all 1's" $ do
        matmatTest1DGEMV
    describe "CGEMV" $ do
      it "2x2 all 1's" $ do
        matmatTest1CGEMV
    describe "ZGEMV" $ do
      it "2x2 all 1's" $ do
        matmatTest1ZGEMV

matmatTest1SGEMV :: IO ()
matmatTest1SGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0::Float))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 :: Float))
    res  <- Matrix.generateMutableDenseVector  2 (\_ -> (0.0 :: Float))
    BLAS.sgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2,2]

matmatTest1DGEMV :: IO ()
matmatTest1DGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector 2  (\_ -> (0.0 ))
    BLAS.dgemv Matrix.NoTranspose 1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2.0,2.0]

matmatTest1CGEMV :: IO ()
matmatTest1CGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector  2 (\_ -> (0.0 ))
    BLAS.cgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2.0,2.0]

matmatTest1ZGEMV :: IO ()
matmatTest1ZGEMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2) (\_ -> (1.0))
    right <- Matrix.generateMutableDenseVector 2 (\_ -> (1.0 ))
    res  <- Matrix.generateMutableDenseVector 2 (\_ -> (0.0 ))
    BLAS.zgemv Matrix.NoTranspose  1.0 1.0 left right res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2.0,2.0]

----
----
gerSpec :: Spec
gerSpec =
  context "?GER" $ do
    describe "SGER" $ do
      it "2x2 all 1's" $ do
        matmatTest1SGER
    describe "DGER" $ do
      it "2x2 all 1's" $ do
        matmatTest1DGER

matmatTest1SGER :: IO ()
matmatTest1SGER = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,2) (\_ -> 1.0)
  x <- Matrix.generateMutableDenseVector 2 (\_ -> 2.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0)
  BLAS.sger 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [13.0,13.0,13.0,13.0]

matmatTest1DGER :: IO ()
matmatTest1DGER = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (2,2) (\_ -> 1.0)
  x <- Matrix.generateMutableDenseVector 2 (\_ -> 2.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0)
  BLAS.sger 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [13.0,13.0,13.0,13.0]

gercSpec :: Spec
gercSpec =
  context "?GERC" $ do
    describe "CGERC" $ do
      it "2x3 all 1+i's" $ do
        matmatTest1CGERC
    describe "ZGERC" $ do
      it "2x3 all 1+i's" $ do
        matmatTest1ZGERC

matmatTest1CGERC :: IO ()
matmatTest1CGERC = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+3.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+2.0)
  BLAS.cgerc 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [25.0:+11.0, 25.0:+11.0, 1.0:+1.0, 25.0:+11.0, 25.0:+11.0, 1.0:+1.0]
  -- why the following is not correct...
  -- resList `shouldBe` [25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0]

matmatTest1ZGERC :: IO ()
matmatTest1ZGERC = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+3.0)
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+2.0)
  BLAS.zgerc 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [25.0:+11.0, 25.0:+11.0, 1.0:+1.0, 25.0:+11.0, 25.0:+11.0, 1.0:+1.0]
  -- why the following is not correct...
  -- resList `shouldBe` [25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0, 25.0:+11.0]

geruSpec :: Spec
geruSpec =
  context "?GERU" $ do
    describe "CGERU" $ do
      it "2x3 all 1+i's" $ do
        matmatTest1CGERU
    describe "ZGERU" $ do
      it "2x3 all 1+i's" $ do
        matmatTest1ZGERU
      

matmatTest1CGERU :: IO ()
matmatTest1CGERU = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+(-3.0))
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+(-2.0))
  BLAS.cgeru 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0, 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0]
  -- why the following is not correct...
  -- resList `shouldBe` [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0)]

matmatTest1ZGERU :: IO ()
matmatTest1ZGERU = do
  res <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3,2) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 2.0:+(-3.0))
  y <- Matrix.generateMutableDenseVector 2 (\_ -> 3.0:+(-2.0))
  BLAS.zgeru 2.0 x y res
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat res
  resList `shouldBe` [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0, 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+1.0]
  -- why the following is not correct...
  -- resList `shouldBe` [1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0), 1.0:+(-25.0)]

-- [1:+0    1:+1    1:+1    0:+0]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [0:+0    1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]

hbmvSpec :: Spec
hbmvSpec =
  context "?HBMV" $ do
    describe "CHBMV" $ do
      it "4*3 a(4x4 matrix) upper all 1+i's" $ do
        matvecTest1CHBMV
      it "4*3 a(4x4 matrix) upper all 1+i's (column oriented)" $ do
        matvecTest2CHBMV
    describe "ZHBMV" $ do
      it "4*3 a(4x4 matrix) lower all 1+i's" $ do
        matvecTest1ZHBMV


matvecTest1CHBMV :: IO ()
matvecTest1CHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 3) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chbmv Matrix.MatUpper 2 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

matvecTest2CHBMV :: IO ()
matvecTest2CHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (4, 3) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chbmv Matrix.MatUpper 2 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

-- [1:+0    1:+1    1:+1    0:+0]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [0:+0    1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]
matvecTest1ZHBMV :: IO ()
matvecTest1ZHBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 3) (\_ -> 1.0:+(-1.0))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.zhbmv Matrix.MatLower 2 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+5.0, 3.0:+5.0, 5.0:+3.0, 5.0:+1.0]

-- [1:+0    1:+1    1:+1    1:+1]
-- [1:+(-1) 1:+0    1:+1    1:+1]
-- [1:+(-1) 1:+(-1) 1:+0    1:+1]
-- [1:+(-1) 1:+(-1) 1:+(-1) 1:+0]
--
-- [1:+1]
-- [1:+1]
-- [1:+1]
-- [1:+1]
--
hemvSpec :: Spec
hemvSpec =
  context "?HEMV" $ do
    describe "CHEMV" $ do
      it "4*3 a(4x4 matrix) upper all 1+i's" $ do
        matvecTest1CHEMV
      it "4*3 a(4x4 matrix) upper all 1+i s (column oriented)" $ do
        matvecTest2CHEMV
    describe "ZHEMV" $ do
      it "4*3 a(4x4 matrix) lower all 1+i's" $ do
        matvecTest1ZHEMV

matvecTest1CHEMV :: IO ()
matvecTest1CHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chemv Matrix.MatUpper 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

matvecTest2CHEMV :: IO ()
matvecTest2CHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.chemv Matrix.MatUpper 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

matvecTest1ZHEMV :: IO ()
matvecTest1ZHEMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+(-1.0))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1.0:+1.0)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 0.0:+0.0)
  BLAS.zhemv Matrix.MatLower 1.0 a x 0.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [1.0:+7.0, 3.0:+5.0, 5.0:+3.0, 7.0:+1.0]

herSpec :: Spec
herSpec = 
  context "?HER" $ do
    describe "CHER" $ do
      it "4*4 a upper all 1+i's" $ do
        matvecTest1CHER
    describe "ZHER" $ do
      it "4*4 a upper all 1+i's" $ do
        matvecTest1ZHER

matvecTest1CHER :: IO ()
matvecTest1CHER = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.cher Matrix.MatUpper 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [3.0:+0.0, 5.0:+1.0,  7.0:+1.0,  9.0:+1.0,
               1.0:+1.0, 9.0:+0.0, 13.0:+1.0, 17.0:+1.0,
               1.0:+1.0, 1.0:+1.0, 19.0:+0.0, 25.0:+1.0,
               1.0:+1.0, 1.0:+1.0,  1.0:+1.0, 33.0:+0.0]

matvecTest1ZHER :: IO ()
matvecTest1ZHER = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zher Matrix.MatLower 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [3.0:+0.0,  1.0:+1.0,  1.0:+1.0,  1.0:+1.0,
               5.0:+1.0,  9.0:+0.0,  1.0:+1.0,  1.0:+1.0,
               7.0:+1.0, 13.0:+1.0, 19.0:+0.0,  1.0:+1.0,
               9.0:+1.0, 17.0:+1.0, 25.0:+1.0, 33.0:+0.0]

her2Spec :: Spec
her2Spec = 
  context "?HER2" $ do
    describe "CHER2" $ do
      it "4*4 a upper all 1+i's" $ do
        matvecTest1CHER2
    describe "ZHER2" $ do
      it "4*4 a upper all 1+i's" $ do
        matvecTest1ZHER2

matvecTest1CHER2 :: IO ()
matvecTest1CHER2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.cher2 Matrix.MatUpper 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [5.0:+0.0,  9.0:+1.0, 13.0:+1.0, 17.0:+1.0,
               1.0:+1.0, 17.0:+0.0, 25.0:+1.0, 33.0:+1.0,
               1.0:+1.0,  1.0:+1.0, 37.0:+0.0, 49.0:+1.0,
               1.0:+1.0,  1.0:+1.0,  1.0:+1.0, 65.0:+0.0]

matvecTest1ZHER2 :: IO ()
matvecTest1ZHER2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 4) (\_ -> 1.0:+1.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zher2 Matrix.MatLower 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [ 5.0:+0.0,  1.0:+1.0,  1.0:+1.0,  1.0:+1.0,
                9.0:+1.0, 17.0:+0.0,  1.0:+1.0,  1.0:+1.0,
               13.0:+1.0, 25.0:+1.0, 37.0:+0.0,  1.0:+1.0,
               17.0:+1.0, 33.0:+1.0, 49.0:+1.0, 65.0:+0.0]


hpmvSpec :: Spec
hpmvSpec =
  context "?HPMV" $ do
    describe "CHPMV" $ do
      it "4*4 a upper (row oriented)" $ do
        matvecTest1CHPMV
    describe "ZHPMV" $ do
      it "4*4 a upper (column oriented)" $ do
        matvecTest1ZHPMV

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
matvecTest1CHPMV :: IO ()
matvecTest1CHPMV = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [2.0:+2.0, 2.0:+2.0, 2.0:+2.0, 2.0:+2.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [3.0:+3.0, 3.0:+3.0, 3.0:+3.0, 3.0:+3.0] !! idx)
  BLAS.chpmv Matrix.SRow Matrix.MatUpper 4 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [ 3.0:+27.0, 15.0:+55.0, 45.0:+49.0, 89.0:+21.0]

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
matvecTest1ZHPMV :: IO ()
matvecTest1ZHPMV = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [2.0:+2.0, 2.0:+2.0, 2.0:+2.0, 2.0:+2.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [3.0:+3.0, 3.0:+3.0, 3.0:+3.0, 3.0:+3.0] !! idx)
  BLAS.zhpmv Matrix.SColumn Matrix.MatUpper 4 1.0 a x 2.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [ 6.0:+46.0, 14.0:+54.0, 44.0:+48.0, 108.0:+24.0]

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

hprSpec :: Spec
hprSpec =
  context "?HPR" $ do
    describe "CHPR" $ do
      it "4*4 a upper (column oriented)" $ do
        matvecTest1CHPR
    describe "ZHPR" $ do
      it "4*4 a lower (row oriented)" $ do
        matvecTest1ZHPR

matvecTest1CHPR :: IO ()
matvecTest1CHPR = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+0.0, 1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0, 5.0:+5.0, 6.0:+6.0, 7.0:+7.0, 8.0:+8.0, 9.0:+9.0] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.chpr Matrix.SColumn Matrix.MatUpper 4 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [2.0:+0.0, 5.0:+1.0, 10.0:+0.0, 9.0:+3.0, 16.0:+4.0, 23.0:+0.0, 14.0:+6.0, 23.0:+7.0,  32.0:+8.0, 41.0:+0.0]

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
matvecTest1ZHPR :: IO ()
matvecTest1ZHPR = do
  a <- Matrix.generateMutableDenseVector 10 (\idx -> [0.0:+(-0.0), 1.0:+(-1.0), 2.0:+(-2.0), 3.0:+(-3.0), 4.0:+(-4.0), 5.0:+(-5.0), 6.0:+(-6.0), 7.0:+(-7.0), 8.0:+(-8.0), 9.0:+(-9.0)] !! idx)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 2.0:+2.0, 3.0:+3.0, 4.0:+4.0] !! idx)
  BLAS.zhpr Matrix.SRow Matrix.MatLower 4 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [2.0:+0.0, 5.0:+(-1.0), 10.0:+0.0, 9.0:+(-3.0), 16.0:+(-4.0), 23.0:+0.0, 14.0:+(-6.0), 23.0:+(-7.0), 32.0:+(-8.0), 41.0:+0.0]


hpr2Spec :: Spec
hpr2Spec =
  context "?HPR2" $ do
    describe "CHRP2" $ do
      it "4*4 a upper (column oriented)" $ do
        matvecTest1CHPR2
    describe "ZHRP2" $ do
      it "4*4 a upper (row oriented)" $ do
        matvecTest1ZHPR2

-- [12:+0 ...]
-- [.       ...]
-- [.       ...]
-- [.       ...]
--
matvecTest1CHPR2 :: IO ()
matvecTest1CHPR2 = do
  a <- Matrix.generateMutableDenseVector 10 (\_ -> 0.0:+0.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+2.0, 1.0:+2.0, 1.0:+2.0, 1.0:+2.0] !! idx)
  BLAS.chpr2 Matrix.SColumn Matrix.MatUpper 4 2.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0]

matvecTest1ZHPR2 :: IO ()
matvecTest1ZHPR2 = do
  a <- Matrix.generateMutableDenseVector 10 (\_ -> 0.0:+0.0)
  x <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+1.0, 1.0:+1.0, 1.0:+1.0, 1.0:+1.0] !! idx)
  y <- Matrix.generateMutableDenseVector 4 (\idx -> [1.0:+2.0, 1.0:+2.0, 1.0:+2.0, 1.0:+2.0] !! idx)
  BLAS.zhpr2 Matrix.SColumn Matrix.MatUpper 4 2.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0, 12.0:+0.0]

-- [1 2 0 0]
-- [2 3 4 0]
-- [0 4 5 6]
-- [0 0 6 7]
--
-- [1 2]
-- [3 4]
-- [5 6]
-- [7 0]
--
-- [5]
-- [11]
-- [17]
-- [15]

sbmvSpec :: Spec
sbmvSpec =
  context "?SBMV" $ do
    describe "SSBMV" $ do
      it "4*4 a upper (row oriented)" $ do
        matvecTest1SSBMV
    describe "DSBMV" $ do
      it "4*4 a lower (column oriented)" $ do
        matvecTest1DSBMV

matvecTest1SSBMV :: IO ()
matvecTest1SSBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (4, 2) (\(x, y) -> [1, 2, 3, 4, 5, 6, 7, 0] !! (y * 4 + x))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 2)
  BLAS.ssbmv Matrix.MatUpper 1 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [5, 11, 17, 15]

-- [1 3 5 7]
-- [2 4 6 0]

matvecTest1DSBMV :: IO ()
matvecTest1DSBMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (4, 2) (\(x, y) -> [1, 3, 5, 7, 2, 4, 6, 0] !! (y * 4 + x))
  x <- Matrix.generateMutableDenseVector 4 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 4 (\_ -> 2)
  BLAS.dsbmv Matrix.MatLower 1 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [5, 11, 17, 15]

spmvSpec :: Spec
spmvSpec =
  context "?SPMV" $ do
    describe "SSPMV" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSPMV
    describe "DSPMV" $ do
      it "3*3 a lower (column oriented)" $ do
        matvecTest1DSPMV

matvecTest1SSPMV :: IO ()
matvecTest1SSPMV = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\_ -> 2)
  BLAS.sspmv Matrix.SRow Matrix.MatUpper 3 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [8, 13, 16]

matvecTest1DSPMV :: IO ()
matvecTest1DSPMV = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\_ -> 2)
  BLAS.dspmv Matrix.SColumn Matrix.MatLower 3 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [8, 13, 16]

sprSpec :: Spec
sprSpec =
  context "?SPR" $ do
    describe "SSPR" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSPR
    describe "DSPR" $ do
      it "3*3 a upper (column oriented)" $ do
        matvecTest1DSPR

matvecTest1SSPR :: IO ()
matvecTest1SSPR = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  BLAS.sspr Matrix.SRow Matrix.MatUpper 3 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [2, 3, 4, 5, 6, 7]

matvecTest1DSPR :: IO ()
matvecTest1DSPR = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  BLAS.dspr Matrix.SRow Matrix.MatUpper 3 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [2, 3, 4, 5, 6, 7]

spr2Spec :: Spec
spr2Spec = 
  context "?SPR2" $ do
    describe "SSPR2" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSPR2
    describe "DSPR2" $ do
      it "3*3 a upper (column oriented)" $ do
        matvecTest1DSPR2
    

matvecTest1SSPR2 :: IO ()
matvecTest1SSPR2 = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 2, 3] !! idx)
  BLAS.sspr2 Matrix.SRow Matrix.MatUpper 3 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [3, 5, 7, 8, 10, 12]

matvecTest1DSPR2 :: IO ()
matvecTest1DSPR2 = do
  a <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 2, 3] !! idx)
  BLAS.dspr2 Matrix.SRow Matrix.MatUpper 3 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector a
  resList `shouldBe` [3, 5, 7, 8, 10, 12]


symvSpec :: Spec
symvSpec =
  context "?SYMV" $ do
    describe "SSYMV" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSYMV
    describe "DSYMV" $ do
      it "3*3 a lower (column oriented)" $ do
        matvecTest1DSYMV

-- [1 2 3]
-- [0 5 6]
-- [0 0 7]
matvecTest1SSYMV :: IO ()
matvecTest1SSYMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1, 2, 3, 0, 5, 6, 0, 0, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\_ -> 2)
  BLAS.ssymv Matrix.MatUpper 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [8, 15, 18]

matvecTest1DSYMV :: IO ()
matvecTest1DSYMV = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [1, 0, 0, 2, 5, 0, 3, 6, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\_ -> 2)
  BLAS.dsymv Matrix.MatLower 1.0 a x 1.0 y
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resList `shouldBe` [8, 15, 18]


syrSpec :: Spec
syrSpec =
  context "?SYR" $ do
    describe "SSYR" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSYR
    describe "DSYR" $ do
      it "3*3 a upper (column oriented)"$ do
        matvecTest1DSYR
-- [1 2 3]
-- [0 5 6]
-- [0 0 7]
matvecTest1SSYR :: IO ()
matvecTest1SSYR = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1, 2, 3, 0, 5, 6, 0, 0, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  BLAS.ssyr Matrix.MatUpper 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [2, 3, 4, 0, 6, 7, 0, 0, 8]

matvecTest1DSYR :: IO ()
matvecTest1DSYR = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [1, 0, 0, 2, 5, 0, 3, 6, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  BLAS.dsyr Matrix.MatLower 1.0 x a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [2, 3, 4, 0, 6, 7, 0, 0, 8]

syr2Spec :: Spec
syr2Spec =
  context "?SYR2" $ do
    describe "SSYR2" $ do
      it "3*3 a upper (row oriented)" $ do
        matvecTest1SSYR2
    describe "DSYR2" $ do
      it "3*3 a upper (column oriented)" $ do
        matvecTest1DSYR2

matvecTest1SSYR2 :: IO ()
matvecTest1SSYR2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SRow) (3, 3) (\(x, y) -> [1, 2, 3, 0, 5, 6, 0, 0, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 2, 3] !! idx)
  BLAS.ssyr2 Matrix.MatUpper 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [3, 5, 7, 0, 9, 11, 0, 0, 13]

matvecTest1DSYR2 :: IO ()
matvecTest1DSYR2 = do
  a <- Matrix.generateMutableDenseMatrix (Matrix.SColumn) (3, 3) (\(x, y) -> [1, 0, 0, 2, 5, 0, 3, 6, 7] !! (y * 3 + x))
  x <- Matrix.generateMutableDenseVector 3 (\_ -> 1)
  y <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 2, 3] !! idx)
  BLAS.dsyr2 Matrix.MatLower 1.0 x y a
  resList <- Matrix.mutableVectorToList $ _bufferDenMutMat a
  resList `shouldBe` [3, 5, 7, 0, 9, 11, 0, 0, 13]


tbmvSpec :: Spec
tbmvSpec =
  context "?TBMV" $ do
    describe "STBMV" $ do
      it "3x3 upper no trans (row oriented)" $ do
        matmatTest1STBMV
    describe "DTBMV" $ do
      it "3x3 lower trans (column oriented)" $ do
        matmatTest1DTBMV
    describe "CTBMV" $ do
      it "3x3 upper conj trans (row oriented)" $ do
        matmatTest1CTBMV
    describe "ZTBMV" $ do
      it "3x3 lower no trans (row oriented)" $ do
        matmatTest1ZTBMV

-- [ 1 2 0]   [-2]   [4]
-- [ 0 1 1] * [ 3] = [5]
-- [ 0 0 1]   [ 2]   [2]
matmatTest1STBMV:: IO ()
matmatTest1STBMV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (3, 2) (\(x, y) -> [0, 2, 1, 1, 1, 1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.stbmv MatUpper NoTranspose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [4, 5, 2]

-- [1 1 0]   [-2]   [1]
-- [0 1 1] * [ 3] = [5]
-- [0 0 1]   [ 2]   [2]
matmatTest1DTBMV:: IO ()
matmatTest1DTBMV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SColumn)  (3, 2) (\(x, y) -> [0, 1, 1, 1, 1, 1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.dtbmv MatLower Transpose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [1, 5, 2]

-- [ 1:+0    0:+0    0:+0]   [-2]   [-2:+0   ]
-- [ 2:+(-2) 1:+0    0:+0] * [ 3] = [-1:+4   ]
-- [ 0:+0    1:+(-1) 1:+0]   [ 2]   [ 5:+(-3)]
matmatTest1CTBMV:: IO ()
matmatTest1CTBMV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (3, 2) (\(x, y) -> [0:+0, 2:+2, 1:+1, 1:+1, 1:+1, 1:+1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.ctbmv MatUpper Matrix.ConjTranspose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [(-2):+0, (-1):+4, 5:+(-3)]

-- [1 1 0]   [2]   [5]
-- [0 1 1] * [3] = [5]
-- [0 0 1]   [2]   [2]
matmatTest1ZTBMV:: IO ()
matmatTest1ZTBMV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SColumn)  (3, 2) (\(x, y) -> [0:+0, 1:+0, 1:+0, 1:+0, 1:+0, 1:+0] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [2, 3, 2] !! idx)
    BLAS.ztbmv MatUpper NoTranspose MatNonUnit 1 a x -- TODO: NAN error when using Lower Transpose
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [5, 5, 2]


tbsvSpec :: Spec
tbsvSpec =
  context "?TBSV" $ do
    describe "STBSV" $ do
      it "3x3 upper no trans (row oriented)" $ do
        matmatTest1STBSV
    describe "DTBSV" $ do
      it "3x3 lower trans (column oriented)" $ do
        matmatTest1DTBSV
    describe "CTBSV" $ do
      it "3x3 upper conj trans (row oriented)" $ do
        matmatTest1CTBSV
    describe "ZTBSV" $ do
      it "3x3 lower no trans (row oriented)" $ do
        matmatTest1ZTBSV
        

-- [ 1 2 0]   [-2]   [4]
-- [ 0 1 1] * [ 3] = [5]
-- [ 0 0 1]   [ 2]   [2]
matmatTest1STBSV:: IO ()
matmatTest1STBSV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (3, 2) (\(x, y) -> [0, 2, 1, 1, 1, 1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [4, 5, 2] !! idx)
    BLAS.stbsv MatUpper NoTranspose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [1 1 0]   [-2]   [1]
-- [0 1 1] * [ 3] = [5]
-- [0 0 1]   [ 2]   [2]
matmatTest1DTBSV:: IO ()
matmatTest1DTBSV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SColumn)  (3, 2) (\(x, y) -> [0, 1, 1, 1, 1, 1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 5, 2] !! idx)
    BLAS.dtbsv MatLower Transpose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [ 1:+0    0:+0    0:+0]   [-2]   [-2:+0   ]
-- [ 2:+(-2) 1:+0    0:+0] * [ 3] = [-1:+4   ]
-- [ 0:+0    1:+(-1) 1:+0]   [ 2]   [ 5:+(-3)]
matmatTest1CTBSV:: IO ()
matmatTest1CTBSV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (3, 2) (\(x, y) -> [0:+0, 2:+2, 1:+1, 1:+1, 1:+1, 1:+1] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [(-2):+0, (-1):+4, 5:+(-3)] !! idx)
    BLAS.ctbsv MatUpper Matrix.ConjTranspose MatUnit 1 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [1 1 0]   [2]   [5]
-- [0 1 1] * [3] = [5]
-- [0 0 1]   [2]   [2]
matmatTest1ZTBSV:: IO ()
matmatTest1ZTBSV = do
    a  <- Matrix.generateMutableDenseMatrix (Matrix.SColumn)  (3, 2) (\(x, y) -> [0:+0, 1:+0, 1:+0, 1:+0, 1:+0, 1:+0] !! (y * 3 + x))
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [5, 5, 2] !! idx)
    BLAS.ztbsv MatUpper NoTranspose MatNonUnit 1 a x -- TODO: NAN error when using Lower Transpose
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [2, 3, 2]


tpmvSpec :: Spec
tpmvSpec =
  context "?TPMV" $ do
    describe "STPMV" $ do
      it "3x3 upper no trans (row oriented)" $ do
        matmatTest1STPMV
    describe "DTPMV" $ do
      it "3x3 lower trans (column oriented)" $ do
        matmatTest1DTPMV
    describe "CTPMV" $ do
      it "3x3 upper conj trans (row oriented)" $ do
        matmatTest1CTPMV
    describe "ZTPMV" $ do
      it "3x3 lower no trans (row oriented)" $ do
        matmatTest1ZTPMV

-- [ 1 2 0]   [-2]   [4]
-- [ 0 1 1] * [ 3] = [5]
-- [ 0 0 1]   [ 2]   [2]
matmatTest1STPMV:: IO ()
matmatTest1STPMV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 0, 1, 1, 1] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.stpmv Matrix.SRow MatUpper NoTranspose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [4, 5, 2]

-- [1 1 0]   [-2]   [1]
-- [0 1 1] * [ 3] = [5]
-- [0 0 1]   [ 2]   [2]
matmatTest1DTPMV:: IO ()
matmatTest1DTPMV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 1, 0, 1, 1, 1] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.dtpmv Matrix.SColumn MatLower Transpose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [1, 5, 2]

-- [ 1:+0    0:+0    0:+0]   [-2]   [-2:+0   ]
-- [ 2:+(-2) 1:+0    0:+0] * [ 3] = [-1:+4   ]
-- [ 0:+0    1:+(-1) 1:+0]   [ 2]   [ 5:+(-3)]
matmatTest1CTPMV:: IO ()
matmatTest1CTPMV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1:+0, 2:+2, 0:+0, 1:+0, 1:+1, 1:+0] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [-2, 3, 2] !! idx)
    BLAS.ctpmv Matrix.SRow MatUpper Matrix.ConjTranspose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [(-2):+0, (-1):+4, 5:+(-3)]

-- [1 1 0]   [2]   [5]
-- [0 1 1] * [3] = [5]
-- [0 0 1]   [2]   [2]
matmatTest1ZTPMV:: IO ()
matmatTest1ZTPMV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1:+0, 1:+0, 1:+0, 0:+0, 1:+0, 1:+0] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [2, 3, 2] !! idx)
    BLAS.ztpmv Matrix.SColumn MatUpper NoTranspose MatNonUnit 3 a x -- TODO: NAN error when using Lower Transpose
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [5, 5, 2]

tpsvSpec :: Spec
tpsvSpec =
  context "?TPSV" $ do
    describe "STPSV" $ do
      it "3x3 upper no trans (row oriented)" $ do
        matmatTest1STPSV
    describe "DTPSV" $ do
      it "3x3 lower trans (column oriented)" $ do
        matmatTest1DTPSV
    describe "CTPSV" $ do
      it "3x3 upper conj trans (row oriented)" $ do
        matmatTest1CTPSV
    describe "ZTPSV" $ do
      it "3x3 lower no trans (row oriented)" $ do
        matmatTest1ZTPSV
        
-- [ 1 2 0]   [-2]   [4]
-- [ 0 1 1] * [ 3] = [5]
-- [ 0 0 1]   [ 2]   [2]
matmatTest1STPSV:: IO ()
matmatTest1STPSV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 2, 0, 1, 1, 1] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [4, 5, 2] !! idx)
    BLAS.stpsv Matrix.SRow MatUpper NoTranspose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [1 1 0]   [-2]   [1]
-- [0 1 1] * [ 3] = [5]
-- [0 0 1]   [ 2]   [2]
matmatTest1DTPSV:: IO ()
matmatTest1DTPSV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1, 1, 0, 1, 1, 1] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [1, 5, 2] !! idx)
    BLAS.dtpsv Matrix.SColumn MatLower Transpose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [ 1:+0    0:+0    0:+0]   [-2]   [-2:+0   ]
-- [ 2:+(-2) 1:+0    0:+0] * [ 3] = [-1:+4   ]
-- [ 0:+0    1:+(-1) 1:+0]   [ 2]   [ 5:+(-3)]
matmatTest1CTPSV:: IO ()
matmatTest1CTPSV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1:+0, 2:+2, 0:+0, 1:+0, 1:+1, 1:+0] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [(-2):+0, (-1):+4, 5:+(-3)]!! idx)
    BLAS.ctpsv Matrix.SRow MatUpper Matrix.ConjTranspose MatUnit 3 a x
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [-2, 3, 2]

-- [1 1 0]   [2]   [5]
-- [0 1 1] * [3] = [5]
-- [0 0 1]   [2]   [2]
matmatTest1ZTPSV:: IO ()
matmatTest1ZTPSV = do
    a  <- Matrix.generateMutableDenseVector 6 (\idx -> [1:+0, 1:+0, 1:+0, 0:+0, 1:+0, 1:+0] !! idx)
    x  <- Matrix.generateMutableDenseVector 3 (\idx -> [5, 5, 2] !! idx)
    BLAS.ztpsv Matrix.SColumn MatUpper NoTranspose MatNonUnit 3 a x -- TODO: NAN error when using Lower Transpose
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
    resList `shouldBe` [2, 3, 2]

trmvSpec :: Spec
trmvSpec =
  context "?TRMV" $ do
    describe "STRMV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1STRMV
    describe "DTRMV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1DTRMV
    describe "CTRMV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1CTRMV
    describe "ZTRMV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1ZTRMV

matmatTest1STRMV:: IO ()
matmatTest1STRMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
            (\(i,j) -> if i >= j then (1.0::Float) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 2 else 1)
    BLAS.strmv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [3,1]

matmatTest1DTRMV:: IO ()
matmatTest1DTRMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::Double) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 2 else 1)
    BLAS.dtrmv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [3,1]

matmatTest1CTRMV:: IO ()
matmatTest1CTRMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Float)) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 2 else 1)
    BLAS.ctrmv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [3,1]

matmatTest1ZTRMV:: IO ()
matmatTest1ZTRMV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Double )) else 0 )
    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 2 else 1)
    BLAS.ztrmv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [3,1]

trsvSpec :: Spec
trsvSpec =
  context "?TRSV" $ do
    describe "STRSV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1STRSV
    describe "DTRSV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1DTRSV
    describe "CTRSV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1CTRSV
    describe "ZTRSV" $ do
      it "2x2 upper 1's" $ do
        matmatTest1ZTRSV

matmatTest1STRSV:: IO ()
matmatTest1STRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
            (\(i,j) -> if i >= j then (1.0::Float) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.strsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2,1]

matmatTest1DTRSV:: IO ()
matmatTest1DTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::Double) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.dtrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2,1]

matmatTest1CTRSV:: IO ()
matmatTest1CTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Float)) else 0 )

    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.ctrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2,1]

matmatTest1ZTRSV:: IO ()
matmatTest1ZTRSV = do
    left  <- Matrix.generateMutableDenseMatrix (Matrix.SRow)  (2,2)
                (\(i,j) -> if i >= j then (1.0::(Complex Double )) else 0 )
    res  <- Matrix.generateMutableDenseVector  2 (\i -> if i == 0 then 3 else 1)
    BLAS.ztrsv MatUpper NoTranspose MatUnit left res
    resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector res
    resList `shouldBe` [2,1]
