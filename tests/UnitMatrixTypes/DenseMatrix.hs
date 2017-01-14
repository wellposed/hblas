module UnitMatrixTypes.DenseMatrix(unitTestDenseMatrix) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Storable as SV

import  Numerical.HBLAS.MatrixTypes as Matrix


{-
 -showColumnMatrixTest :: IO ()
 -showColumnMatrixTest = do
 -    let matrix = Matrix.DenseMatrix Matrix.SColumn 3 2 2 $ SV.fromList ([1..6] :: [Float])
 -    matrix @?= DenseMatrix SColumn  3 2 2 (SV.fromList [1.0,2.0,3.0,4.0,5.0,6.0])
 -
 -showRowMatrixTest :: IO ()
 -showRowMatrixTest = do
 -    let matrix = Matrix.DenseMatrix Matrix.SRow 3 2 3 $ SV.fromList ([1..6] :: [Float])
 -    matrix @?= DenseMatrix SRow  3 2 3 (SV.fromList [1.0,2.0,3.0,4.0,5.0,6.0])
 -}

unitTestDenseMatrix = testGroup "DenseMatrix tests " []
    {-
     -[ testCase "show on column oriented matrix" showColumnMatrixTest
     -, testCase "show on row oriented matrix" showRowMatrixTest
     -]
     -}

