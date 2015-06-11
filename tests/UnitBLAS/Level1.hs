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

matTest1SASUM :: IO ()
matTest1SASUM = do
  mat <- Matrix.generateMutableDenseVector 6 (\idx -> [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] !! idx)
  res <- BLAS.sasum 6 mat 1
  res @?= 21.0

matTest2SASUM :: IO ()
matTest2SASUM = do
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

unitTestLevel1BLAS = testGroup "BlAS Level 1 tests " [
                     testCase "sasum on 6 with incx 1" matTest1SASUM,
                     testCase "sasum on 12 with incx 2" matTest2SASUM,
                     testCase "saxpy on 6 and 6 with both incx 1" vecTest1SAXPY,
                     testCase "saxpy on 12 and 18 with incx 2 and 3" vecTest2SAXPY,
                     testCase "dcopy on 6 and 6 with both incx 1" vecTest1DCOPY,
                     testCase "dcopy on 6 and 9 with incx 2 and 3" vecTest2DCOPY
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
