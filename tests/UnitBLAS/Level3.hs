-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module UnitBLAS.Level3(unitTestLevel3BLAS) where 

import Test.HUnit
--import Numerical.Array.Shape as S 
import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Storable as SV 
import qualified Data.Vector.Storable.Mutable as SMV 

import Data.Complex

import  Numerical.HBLAS.MatrixTypes as Matrix 
import  Numerical.HBLAS.BLAS as BLAS 

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
    resList @?= replicate 9 3

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



