-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module HBLAS.BLAS.Level1Spec (main, spec) where

import Data.Complex

import Numerical.HBLAS.MatrixTypes as Matrix
import Numerical.HBLAS.BLAS.Level1 as BLAS

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  asumSpec
  axpySpec
  copySpec
  dotSpec
  sdotSpec
  dotcSpec
  dotuSpec
  nrm2Spec
  rotSpec
  rotgSpec
  rotmSpec
  rotmgSpec
  scalSpec
  swapSpec
  iamaxSpec
  iaminSpec

asumSpec :: Spec
asumSpec =
  context "?ASUM" $ do
    describe "SASUM" $ do
      it "vector of length 6 with incx 1" $ do
        vecTest1SASUM
      it "vector of length 12 with incx 2" $ do
        vecTest2SASUM

vecTest1SASUM :: IO ()
vecTest1SASUM = do
  vec <- Matrix.generateMutableDenseVector 6 (\idx -> [1 .. 6] !! idx)
  res <- BLAS.sasum vec
  res `shouldBe` (21.0 :: Float)

vecTest2SASUM :: IO ()
vecTest2SASUM = do
  vec <- Matrix.generateMutableDenseVectorWithStride 12 2 (\idx -> [1 .. 12] !! idx)
  res <- BLAS.sasum  vec
  res `shouldBe` (sum [1,3 .. 12] :: Float)

axpySpec :: Spec
axpySpec =
  context "?AXPY" $ do
    describe "SAXPY" $ do
      it "vectors of lengths 6 and 6 with both incx 1" $ do
        vecTest1SAXPY
      it "vectors of lengths 12 and 18 with incx 2 and 3" $ do
        vecTest2SAXPY

vecTest1SAXPY :: IO ()
vecTest1SAXPY = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1 .. 6] !! idx)
  output <- Matrix.generateMutableDenseVector 6 (\idx -> [2, 3, 4, 3, 5, 6] !! idx)
  BLAS.saxpy 6 (-1.0) input output
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList `shouldBe` [1, 1, 1, -1, 0, 0]


vecTest2SAXPY :: IO ()
vecTest2SAXPY = do
  input <- Matrix.generateMutableDenseVectorWithStride 18 3 (\idx -> [1 .. 18] !! idx)
  output <- Matrix.generateMutableDenseVectorWithStride 12 2 (\idx -> [1 .. 12] !! idx)
  BLAS.saxpy 6 2.0 input output
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList `shouldBe` [3, 2, 11, 4, 19, 6, 27, 8, 35, 10, 43, 12]


copySpec :: Spec
copySpec =
  context "?COPY" $ do
    describe "DCOPY" $ do
      it "vectors of lengths 6 and 6 with both incx 1" $ do
        vecTest1DCOPY
      it "vectors of lengths 6 and 9 with incx 2 and 3" $ do
        vecTest2DCOPY

vecTest1DCOPY :: IO ()
vecTest1DCOPY = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1 .. 6] !! idx)
  output <- Matrix.generateMutableDenseVector 6 (const 0.0)
  BLAS.dcopy 6 input output
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList `shouldBe` [1, 2, 3, 4, 5, 6]

vecTest2DCOPY :: IO ()
vecTest2DCOPY = do
  input <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1 .. 6] !! idx)
  output <- Matrix.generateMutableDenseVectorWithStride 9 3 (const 0.0)
  BLAS.dcopy 3 input output
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList `shouldBe` [1, 0, 0, 3, 0, 0, 5, 0, 0]


dotSpec :: Spec
dotSpec =
  context "?DOT" $ do
    describe "SDOT" $ do
      it "vectors of lengths 6 and 12 with incx 2 and 4" $ do
        vecTest1SDOT
    describe "DDOT" $ do
      it "vectors of lengths 12 and 6 with incx 2 and 1" $ do
        vecTest1DDOT

vecTest1SDOT :: IO ()
vecTest1SDOT = do
  left <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 12 4 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  res <- sdot 3 left right
  res `shouldBe` ((1 + 15 + 45) :: Float)

vecTest1DDOT :: IO ()
vecTest1DDOT = do
  left <- Matrix.generateMutableDenseVectorWithStride 12 2 (((+) 1) . fromRational . toRational)
  right <- Matrix.generateMutableDenseVectorWithStride 6 1 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  res <- ddot 6 left right
  res `shouldBe` ((1 + 6 + 15 + 28 + 45 + 66) :: Double)

sdotSpec :: Spec
sdotSpec =
  context "?SDOT" $ do
    describe "SDSDOT" $ do
      it "vectors of lengths 6 and 12 with incx 2 and 4" $ do
        pending
    describe "DSDOT" $ do
      it "vectors of lengths 12 and 6 with incx 2 and 1" $ do
        vecTest1DSDOT

{-vecTest1SDSDOT :: IO ()-}
{-vecTest1SDSDOT = do-}
  {-left <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> ([1 .. 6] :: [Float]) !! idx) -- Float-}
  {-right <- Matrix.generateMutableDenseVectorWithStride 12 4 (\idx -> ([1 .. 12] :: [Float]) !! idx) -- Float-}
  {-res <- sdsdot 3 2.0 left right-}
  {-res `shouldBe` ((2 :: Float) + 1 + 15 + 45)-}

vecTest1DSDOT :: IO ()
vecTest1DSDOT = do
  left <- Matrix.generateMutableDenseVectorWithStride 12 2 (\idx -> ([1 .. 12] :: [Float]) !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 6 1 (\idx -> ([1 .. 6] :: [Float]) !! idx)
  res <- dsdot 6 left right
  res `shouldBe` ((1 + 6 + 15 + 28 + 45 + 66) :: Double)


dotcSpec :: Spec
dotcSpec =
  context "?DOTC" $ do
    describe "CDOTC" $ do
      it "vectors of length 6 and 9 with incx 2 and 3" $ do
        vecTest1CDOTC


vecTest1CDOTC :: IO ()
vecTest1CDOTC = do
  left <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [2:+3, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 9 3 (\idx -> [1:+(-2), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  cdotc 3 left right `shouldReturn` ((-2):+(-9) :: Complex Float)


dotuSpec :: Spec
dotuSpec =
  context "?DOTU" $ do
    describe "CDOTU" $ do
      it "vectors of length 6 and 9 with incx of 2 and 3" $ do
        vecTest1CDOTU

vecTest1CDOTU :: IO ()
vecTest1CDOTU = do
  left <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 9 3 (\idx -> [1:+(-2), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  --res <- Matrix.generateMutableValue (1:+1)
  res <- cdotu 3 left right

  res `shouldBe` 5:+1

nrm2Spec :: Spec
nrm2Spec =
  context "?NRM2" $ do
    describe "SNRM2" $ do
      it "vector of length 6 with incx of 1" $ do
        vecTest1SNRM2
    describe "DZNRM2" $ do
      it "vector of length 8 with incx of 2" $ do
        vecTest1DZNRM2

vecTest1SNRM2 :: IO ()
vecTest1SNRM2 = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, -2.0, 3.0, -4.0, 5.0, -6.0] !! idx)
  res <- snrm2 6 input
  True `shouldBe` 1e-6 > (abs $ res - (sqrt $ sum $ fmap (\x->x^2) [1, 2, 3, 4, 5, 6]))

vecTest1DZNRM2 :: IO ()
vecTest1DZNRM2 = do
  input <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1] !! idx)
  res <- dznrm2 4 input
  True `shouldBe` 1e-12 > (abs $ res - (sqrt $ sum $ fmap (\x->x^2) [1, 1, 2, 3, 3, 1, 4, 2]))

rotSpec :: Spec
rotSpec =
  context "?ROT" $ do
    describe "SROT" $ do
      it "vectors of length 6 and 6 with incx of 2" $ do
        vecTest1SROT
    describe "DROT" $ do
      it "vectors of length 4 and 8 with incx of 1 and 2" $ do
        vecTest1DROT


vecTest1SROT :: IO ()
vecTest1SROT = do
  left <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [6.0, 5.0, 4.0, 3.0, 2.0, 1.0] !! idx)
  srot 3 left right (-1) 2
  resLeft <- Matrix.mutableVectorToList $ _bufferMutDenseVector left
  resRight <- Matrix.mutableVectorToList $ _bufferMutDenseVector right
  resLeft `shouldBe` [11.0, 2.0, 5.0, 4.0, -1.0, 6.0]
  resRight `shouldBe` [-8.0, 5.0, -10.0, 3.0, -12.0, 1.0]

vecTest1DROT :: IO ()
vecTest1DROT = do
  left <- Matrix.generateMutableDenseVectorWithStride 4 1 (\idx -> [1, 2, 3, 4] !! idx)
  right <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [8, 7, 6, 5, 4, 3, 2, 1] !! idx)
  drot 4 left right 0 (-2)
  resLeft <- Matrix.mutableVectorToList $ _bufferMutDenseVector left
  resRight <- Matrix.mutableVectorToList $ _bufferMutDenseVector right
  resLeft `shouldBe` [-16, -12, -8, -4]
  resRight `shouldBe` [2, 7, 4, 5, 6, 3, 8, 1]

rotgSpec :: Spec
rotgSpec =
  context "?ROTG" $ do
    describe "SROTG" $ do
      it "apply on 3 and 4" $ do
        pending
    describe "DROTG" $ do
      it "apply on 5.8 and 3.4" $ do
        pending

{-vecTest1SROTG :: IO ()-}
{-vecTest1SROTG = do-}
  {-a <- Matrix.generateMutableValue 3-}
  {-b <- Matrix.generateMutableValue 4-}
  {-c <- Matrix.generateMutableValue 0-}
  {-s <- Matrix.generateMutableValue 0-}
  {-srotg a b c s-}
  {-av <- Matrix.mutableValueToValue a-}
  {-bv <- Matrix.mutableValueToValue b-}
  {-cv <- Matrix.mutableValueToValue c-}
  {-sv <- Matrix.mutableValueToValue s-}
  {-av `shouldBe` 5-}
  {-True `shouldBe` 1e-6 > (abs $ bv - 1/0.6)-}
  {-cv `shouldBe` 0.6-}
  {-sv `shouldBe` 0.8-}

{-vecTest1DROTG :: IO ()-}
{-vecTest1DROTG = do-}
  {-a <- Matrix.generateMutableValue 5.8-}
  {-b <- Matrix.generateMutableValue 3.4-}
  {-c <- Matrix.generateMutableValue 0-}
  {-s <- Matrix.generateMutableValue 0-}
  {-drotg a b c s-}
  {-av <- Matrix.mutableValueToValue a-}
  {-bv <- Matrix.mutableValueToValue b-}
  {-cv <- Matrix.mutableValueToValue c-}
  {-sv <- Matrix.mutableValueToValue s-}
  {-True `shouldBe` 1e-12 > (abs $ av - sqrt(3.4^2 + 5.8^2))-}
  {-True `shouldBe` 1e-12 > (abs $ bv - 3.4 / sqrt(3.4^2 + 5.8^2))-}
  {-True `shouldBe` 1e-12 > (abs $ cv - 5.8 / sqrt(3.4^2 + 5.8^2))-}
  {-True `shouldBe` 1e-12 > (abs $ sv - 3.4 / sqrt(3.4^2 + 5.8^2))-}


rotmSpec :: Spec
rotmSpec =
  context "?ROTM" $ do
    it "vectors of length 4 and 8 with incx of 1 and 2, param starts with -1" $ do
      pending
    it "vectors of length 6 and 9 with incx of 2 and 3, param starts with 1" $ do
      pending

{-
vecTest1DROTM :: IO ()
vecTest1DROTM = do
  x <- Matrix.generateMutableDenseVectorWithStride 4 1 (\idx -> [1, 2, 3, 4] !! idx)
  y <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [8, 7, 6, 5, 4, 3, 2, 1] !! idx)
  param <- Matrix.generateMutableDenseVector 5 (\idx -> [-1, 0, -1, 1, 0] !! idx)
  drotm 4 x y param
  resX <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  resY <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resX `shouldBe` [8, 6, 4, 2]
  resY `shouldBe` [-1, 7, -2, 5, -3, 3, -4, 1]

vecTest1SROTM :: IO ()
vecTest1SROTM = do
  x <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1, 2, 3, 4, 5, 6] !! idx)
  y <- Matrix.generateMutableDenseVectorWithStride 9 3 (\idx -> [9, 8, 7, 6, 5, 4, 3, 2, 1] !! idx)
  param <- Matrix.generateMutableDenseVector 5 (\idx -> [1, 1, 2, -2, 1] !! idx)
  srotm 3 x y param
  resX <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  resY <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  resX `shouldBe` [10, 2, 9, 4, 8, 6]
  resY `shouldBe` [8, 8, 7, 3, 5, 4, -2, 2, 1]
-}


rotmgSpec :: Spec
rotmgSpec =
  context "?ROTMG" $ do
    describe "SROTMG" $ do
      it "todo" $ do
        pending

{-
vecTest1SROTMG :: IO ()
vecTest1SROTMG = do
  d1 <- Matrix.generateMutableValue 3
  d2 <- Matrix.generateMutableValue 6
  x <- Matrix.generateMutableValue 1
  let y = 1
  param <- Matrix.generateMutableDenseVector 5 (\idx -> [-1, 1, 1, -1, 1] !! idx)
  srotmg d1 d2 x y param
  paramR <- Matrix.mutableVectorToList $ _bufferMutDenseVector param
  updatedD1 <- Matrix.mutableValueToValue d1
  updatedD2 <- Matrix.mutableValueToValue d2
  updatedX <- Matrix.mutableValueToValue x
  paramR `shouldBe` [1, 0, 0.5, 0, 1]
  updatedD1 `shouldBe` 4
  updatedD2 `shouldBe` 2
  updatedX `shouldBe` 1.5
-}



scalSpec :: Spec
scalSpec =
  context "?SCAL" $ do
    describe "SSCAL" $ do
      it "vector of length 8 with incx 2" $ do
        vecTest1SSCAL
    describe "CSCAL" $ do
      it "vector of length 8 with incx 4" $ do
        vecTest1CSCAL
    describe "CSSCAL" $ do
      it "vector of length 8 with incx 1" $ do
        vecTest1CSSCAL

vecTest1SSCAL :: IO ()
vecTest1SSCAL = do
  x <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [1, 2, 3, 4, 5, 6, 7, 8] !! idx)
  sscal 4 (-2) x
  xRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  xRes `shouldBe` [-2, 2, -6, 4, -10, 6, -14, 8]


vecTest1CSCAL :: IO ()
vecTest1CSCAL = do
  x <- Matrix.generateMutableDenseVectorWithStride 8 4 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1] !! idx)
  cscal 2 (2:+(-2)) x -- size 2?
  xRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  xRes `shouldBe` [4:+0, 1:+2, 2:+(-3), 2:+(-2), (-4):+8, (-3):+0, (-4):+2, (-4):+1]

vecTest1CSSCAL :: IO ()
vecTest1CSSCAL = do
  x <- Matrix.generateMutableDenseVector 8 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1] !! idx)
  csscal 8 (-2) x
  xRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  xRes `shouldBe` [(-2):+(-2), (-2):+(-4), (-4):+6, (-4):+4, 6:+(-2), 6:+0, 8:+(-4), 8:+(-2)]

swapSpec :: Spec
swapSpec =
  context "?SWAP" $ do
    describe "SSWAP" $ do
      it "vectors of length 8 and 4 with incx 2 and 1" $ do
        vecTest1SSWAP
    describe "CSWAP" $ do
      it "vectors of length 9 and 6 with incx 3 and 2" $ do
        vecTest1CSWAP

vecTest1SSWAP :: IO ()
vecTest1SSWAP = do
  x <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [1, 2, 3, 4, 5, 6, 7, 8] !! idx)
  y <- Matrix.generateMutableDenseVectorWithStride 4 1 (\idx -> [-1, -2, -3, -4] !! idx)
  sswap 4 x y
  xRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  yRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  xRes `shouldBe` [-1, 2, -2, 4, -3, 6, -4, 8]
  yRes `shouldBe` [1, 3, 5, 7]

vecTest1CSWAP :: IO ()
vecTest1CSWAP = do
  x <- Matrix.generateMutableDenseVectorWithStride 9 3 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1, 0:+9] !! idx)
  y <- Matrix.generateMutableDenseVectorWithStride 6 2 (\idx -> [1:+2, 1:+3, 3:+(-3), 2:+2, 3:+1, 3:+3] !! idx)
  cswap 3 x y
  xRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector x
  yRes <- Matrix.mutableVectorToList $ _bufferMutDenseVector y
  xRes `shouldBe` [1:+2, 1:+2, 2:+(-3), 3:+(-3), (-3):+1, (-3):+0, 3:+1, (-4):+1, 0:+9]
  yRes `shouldBe` [1:+1, 1:+3, 2:+(-2), 2:+2, (-4):+2, 3:+3]

iamaxSpec :: Spec
iamaxSpec =
  context "I?AMAX" $ do
    describe "ISAMAX" $ do
      it "vector of length 8 with incx 2" $ do
        vecTest1ISAMAX
    describe "ICAMAX" $ do
      it "vector of length 9 with incx 1" $ do
        vecTest1ICAMAX

vecTest1ISAMAX :: IO ()
vecTest1ISAMAX = do
  x <- Matrix.generateMutableDenseVectorWithStride 8 2 (\idx -> [1, 2, 3, 4, 5, 6, 7, 8] !! idx)
  idx <- isamax  x
  idx `shouldBe` 7

vecTest1ICAMAX :: IO ()
vecTest1ICAMAX = do
  x <- Matrix.generateMutableDenseVector 9 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1, 0:+9] !! idx)
  idx <- icamax  x
  idx `shouldBe` 8


iaminSpec :: Spec
iaminSpec =
  context "I?AMIN" $ do
    describe "ISAMIN" $ do
      it "vector of length 8 with incx 2" $ do
        pending
    describe "ICAMIN" $ do
      it "vector of length 9 with incx 1" $ do
        pending

{-
vecTest1ISAMIN :: IO ()
vecTest1ISAMIN = do
  x <- Matrix.generateMutableDenseVector 8 (\idx -> [1, 2, 3, 4, -5, 6, 7, 8] !! idx)
  idx <- isamin 4 x 2
  idx `shouldBe` 2

vecTest1ICAMIN :: IO ()
vecTest1ICAMIN = do
  x <- Matrix.generateMutableDenseVector 9 (\idx -> [1:+2, 1:+2, (-2):+(-3), 2:+(-2), (-3):+1, (-2):+0, (-4):+2, (-4):+1, 0:+9] !! idx)
  idx <- icamin 9 x 1
  idx `shouldBe` 5
-}
