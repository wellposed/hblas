-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module UnitBLAS.Level2(unitTestLevel2BLAS) where 

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


--unitTestShape = testGroup "Shape Unit tests"
--    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )
--    , testCase "foldr on shape" $ ( S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldr  (+) 0  [1,2,3])  )
--    , testCase "scanr1 on shape" (S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) )
--    , testCase "scanl1 on shape" (S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) )
--    ]

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

unitTestLevel2BLAS = testGroup "BlAS Level 2 tests " [
----- gemv tests 
    testCase "sgemv on 2x2 all 1s"    matmatTest1SGEMV
    ,testCase "dgemv  on 2x2 all 1s " matmatTest1DGEMV
    ,testCase "cgemv  on 2x2 all 1s" matmatTest1CGEMV
    ,testCase "zgemv on 2x2 all 1s" matmatTest1ZGEMV
----- trsv tests
    ,testCase "strsv on 2x2 upper 1s" matmatTest1STRSV
    ,testCase "dtrsv on 2x2 upper 1s" matmatTest1DTRSV
    ,testCase "ctrsv on 2x2 upper 1s" matmatTest1CTRSV
    ,testCase "ztrsv on 2x2 upper 1s" matmatTest1ZTRSV

    ]
    