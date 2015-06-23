-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module UnitBLAS.Level1(unitTestLevel1BLAS) where 

--import Test.HUnit
--import Numerical.Array.Shape as S 
import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV

import Data.Complex

import  Numerical.HBLAS.MatrixTypes as Matrix
import  Numerical.HBLAS.BLAS as BLAS

vecTest1SASUM :: IO ()
vecTest1SASUM = do
  mat <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  res <- BLAS.sasum 6 mat 1
  res @?= 21.0

vecTest2SASUM :: IO ()
vecTest2SASUM = do
  mat <- Matrix.generateMutableDenseVector 12 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  res <- BLAS.sasum 6 mat 2
  res @?= 36.0

vecTest1SAXPY :: IO ()
vecTest1SAXPY = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  output <- Matrix.generateMutableDenseVector 6 (\idx -> [2.0, 3.0, 4.0, 3.0, 5.0, 6.0] !! idx)
  BLAS.saxpy 6 (-1.0) input 1 output 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList @?= [1, 1, 1, -1, 0, 0]

vecTest2SAXPY :: IO ()
vecTest2SAXPY = do
  input <- Matrix.generateMutableDenseVector 18 (\idx -> [2.0, 0.0, 0.0,
                                                          3.0, 0.0, 0.0,
                                                          -4.0, 0.0, 0.0,
                                                          -3.0, 0.0, 0.0,
                                                          -5.0, 0.0, 0.0,
                                                          -6.0, 0.0, 0.0] !! idx)
  output <- Matrix.generateMutableDenseVector 12 (\idx -> [-1.0, 0.0,
                                                           -2.0, 0.0,
                                                           3.0, 0.0,
                                                           4.0, 0.0,
                                                           5.0, 0.0,
                                                           6.0, 0.0] !! idx)
  BLAS.saxpy 6 2.0 input 3 output 2
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList @?= [3, 0, 4, 0, -5, 0, -2, 0, -5, 0, -6, 0]

vecTest1DCOPY :: IO ()
vecTest1DCOPY = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  output <- Matrix.generateMutableDenseVector 6 (const 0.0)
  BLAS.dcopy 6 input 1 output 1
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList @?= [1, 2, 3, 4, 5, 6]

vecTest2DCOPY :: IO ()
vecTest2DCOPY = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  output <- Matrix.generateMutableDenseVector 9 (const 0.0)
  BLAS.dcopy 3 input 2 output 3
  resList <- Matrix.mutableVectorToList $ _bufferMutDenseVector output
  resList @?= [1, 0, 0, 3, 0, 0, 5, 0, 0]

vecTest1SDOT :: IO ()
vecTest1SDOT = do 
  left <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  right <- Matrix.generateMutableDenseVector 12 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  res <- sdot 3 left 2 right 4
  res @?= 1 + 15 + 45

vecTest1DDOT :: IO ()
vecTest1DDOT = do 
  left <- Matrix.generateMutableDenseVector 12 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  right <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  res <- ddot 6 left 2 right 1
  res @?= 1 + 6 + 15 + 28 + 45 + 66

vecTest1SDSDOT :: IO ()
vecTest1SDSDOT = do 
  left <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  right <- Matrix.generateMutableDenseVector 12 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  res <- sdsdot 3 2.0 left 2 right 4
  res @?= 2 + 1 + 15 + 45

vecTest1DSDOT :: IO ()
vecTest1DSDOT = do 
  left <- Matrix.generateMutableDenseVector 12 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0] !! idx)
  right <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  res <- dsdot 6 left 2 right 1
  res @?= 1 + 6 + 15 + 28 + 45 + 66

vecTest1CDOTU :: IO ()
vecTest1CDOTU = do
  left <- Matrix.generateMutableDenseVector 6 (\idx -> [1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  right <- Matrix.generateMutableDenseVector 9 (\idx -> [1:+(-2), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  res <- Matrix.generateMutableValue (1:+1)
  cdotu 3 left 2 right 3 res
  resValue <- Matrix.mutableValueToValue res
  resValue @?= 5:+1

vecTest1CDOTC :: IO ()
vecTest1CDOTC = do
  left <- Matrix.generateMutableDenseVector 6 (\idx -> [2:+3, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  right <- Matrix.generateMutableDenseVector 9 (\idx -> [1:+(-2), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1), 1:+1, 1:+(-1)] !! idx)
  res <- Matrix.generateMutableValue (1:+1)
  cdotc 3 left 2 right 3 res
  resValue <- Matrix.mutableValueToValue res
  resValue @?= (-2):+(-9)

vecTest1SNRM2 :: IO ()
vecTest1SNRM2 = do
  input <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, -2.0, 3.0, -4.0, 5.0, -6.0] !! idx)
  res <- snrm2 6 input 1
  True @?= 1e-6 > (abs $ res - (sqrt $ sum $ fmap (\x->x^2) [1, 2, 3, 4, 5, 6]))

vecTest1DZNRM2 :: IO ()
vecTest1DZNRM2 = do
  input <- Matrix.generateMutableDenseVector 8 (\idx -> [1:+1, 1:+2, 2:+(-3), 2:+(-2), (-3):+1, (-3):+0, (-4):+2, (-4):+1] !! idx)
  res <- dznrm2 4 input 2
  True @?= 1e-12 > (abs $ res - (sqrt $ sum $ fmap (\x->x^2) [1, 1, 2, 3, 3, 1, 4, 2]))

vecTest1SROT :: IO ()
vecTest1SROT = do
  left <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  right <- Matrix.generateMutableDenseVector 6 (\idx -> [6.0, 5.0, 4.0, 3.0, 2.0, 1.0] !! idx)
  srot 3 left 2 right 2 (-1) 2
  resLeft <- Matrix.mutableVectorToList $ _bufferMutDenseVector left
  resRight <- Matrix.mutableVectorToList $ _bufferMutDenseVector right
  resLeft @?= [11.0, 2.0, 5.0, 4.0, -1.0, 6.0]
  resRight @?= [-8.0, 5.0, -10.0, 3.0, -12.0, 1.0]

vecTest1DROT :: IO ()
vecTest1DROT = do
  left <- Matrix.generateMutableDenseVector 4 (\idx -> [1, 2, 3, 4] !! idx)
  right <- Matrix.generateMutableDenseVector 8 (\idx -> [8, 7, 6, 5, 4, 3, 2, 1] !! idx)
  drot 4 left 1 right 2 0 (-2)
  resLeft <- Matrix.mutableVectorToList $ _bufferMutDenseVector left
  resRight <- Matrix.mutableVectorToList $ _bufferMutDenseVector right
  resLeft @?= [-16, -12, -8, -4]
  resRight @?= [2, 7, 4, 5, 6, 3, 8, 1]

vecTest1SROTG :: IO ()
vecTest1SROTG = do
  a <- Matrix.generateMutableValue 3
  b <- Matrix.generateMutableValue 4
  c <- Matrix.generateMutableValue 0
  s <- Matrix.generateMutableValue 0
  srotg a b c s
  av <- Matrix.mutableValueToValue a
  bv <- Matrix.mutableValueToValue b
  cv <- Matrix.mutableValueToValue c
  sv <- Matrix.mutableValueToValue s
  av @?= 5
  True @?= 1e-6 > (abs $ bv - 1/0.6)
  cv @?= 0.6
  sv @?= 0.8

vecTest1DROTG :: IO ()
vecTest1DROTG = do
  a <- Matrix.generateMutableValue 5.8
  b <- Matrix.generateMutableValue 3.4
  c <- Matrix.generateMutableValue 0
  s <- Matrix.generateMutableValue 0
  drotg a b c s
  av <- Matrix.mutableValueToValue a
  bv <- Matrix.mutableValueToValue b
  cv <- Matrix.mutableValueToValue c
  sv <- Matrix.mutableValueToValue s
  True @?= 1e-12 > (abs $ av - sqrt(3.4^2 + 5.8^2))
  True @?= 1e-12 > (abs $ bv - 3.4 / sqrt(3.4^2 + 5.8^2))
  True @?= 1e-12 > (abs $ cv - 5.8 / sqrt(3.4^2 + 5.8^2))
  True @?= 1e-12 > (abs $ sv - 3.4 / sqrt(3.4^2 + 5.8^2))

unitTestLevel1BLAS = testGroup "BlAS Level 1 tests " [
                     testCase "sasum on vector of length 6 with incx 1" vecTest1SASUM,
                     testCase "sasum on vector of length 12 with incx 2" vecTest2SASUM,

                     testCase "saxpy on vectors of lengths 6 and 6 with both incx 1" vecTest1SAXPY,
                     testCase "saxpy on vectors of lenghts 12 and 18 with incx 2 and 3" vecTest2SAXPY,

                     testCase "dcopy on vectors of lengths 6 and 6 with both incx 1" vecTest1DCOPY,
                     testCase "dcopy on vectors of lengths 6 and 9 with incx 2 and 3" vecTest2DCOPY,

                     testCase "sdot on vectors of lengths 6 and 12 with incx 2 and 4" vecTest1SDOT,
                     testCase "ddot on vectors of lengths 12 and 6 with incx 2 and 1" vecTest1DDOT,
                     testCase "sdsdot on vectors of lengths 6 and 12 with incx 2 and 4" vecTest1SDSDOT,
                     testCase "dsdot on vectors of 12 and 6 with incx 2 and 1" vecTest1DSDOT,

                     testCase "cdotu on vectors of 6 and 9 with incx of 2 and 3" vecTest1CDOTU,
                     testCase "cdotc on vectors of 6 and 9 with incx of 2 and 3" vecTest1CDOTC,

                     testCase "snrm on vector of length 6 with incx of 1" vecTest1SNRM2,
                     testCase "dznrm on vector of length 8 with incx of 2" vecTest1DZNRM2,

                     testCase "srot on vector of length 6 and 6 with incx of 2" vecTest1SROT,
                     testCase "drot on vector of length 4 and 8 with incx of 1 and 2" vecTest1DROT,
                     
                     testCase "srotg on 3 4" vecTest1SROTG,
                     testCase "drotg on 5.8 3.4" vecTest1DROTG
                     ]

--unitTestShape = testGroup "Shape Unit tests"
--    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )
--    , testCase "foldr on shape" $ ( S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldr  (+) 0  [1,2,3])  )
--    , testCase "scanr1 on shape" (S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) )
--    , testCase "scanl1 on shape" (S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) )
--    ]

{-

import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.BLAS
import Numerical.HBLAS.MatrixTypes 
import Data.Vector.Storable.Mutable as M 
import qualified Data.Vector.Storable as S 

main :: IO ()
main = do
  -- Just test that the symbol resolves
  --openblas_set_num_threads_unsafe 7  
  v  :: IOVector Double <- M.replicate 10 1.0
  res <- unsafeWith v (\ptr-> cblas_ddot_unsafe 10 ptr 1   ptr 1) 


  -}
