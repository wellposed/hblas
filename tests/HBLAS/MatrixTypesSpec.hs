module HBLAS.MatrixTypesSpec(main, spec) where

import Test.Hspec

import qualified Data.Vector.Storable as SV

import  Numerical.HBLAS.MatrixTypes as Matrix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Column Matrix" $
    it "show on column oriented matrix"
      pending
  describe "Row Matrix" $
    it "show on row oriented matrix"
      pending

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
