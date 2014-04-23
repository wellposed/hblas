module Main where

 
import   UnitBLAS.Level1
import   UnitBLAS.Level2
import   UnitBLAS.Level3

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "BLAS Unit Tests"    [unitTestLevel1BLAS, unitTestLevel2BLAS, unitTestLevel3BLAS] -- [unitTestShape] -- , unitTestLayout ]


--unitTests = testGroup "Unit tests"
--  [ testCase "List comparison (different length)" $
--      [1, 2, 3] `compare` [1,2] @?= GT

--  -- the following test does not hold
--  , testCase "List comparison (same length)" $
--      [1, 2, 3] `compare` [1,2,2] @?= LT
--  ]