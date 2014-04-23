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



unitTestLevel3BLAS = testGroup "BlAS Level 3 tests " []

--unitTestShape = testGroup "Shape Unit tests"
--    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )
--    , testCase "foldr on shape" $ ( S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldr  (+) 0  [1,2,3])  )
--    , testCase "scanr1 on shape" (S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) )
--    , testCase "scanl1 on shape" (S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) )
--    ]

{-
turn the following into the first level 2 test

-- {-# LANGUAGE ScopedTypeVariables, DataKinds, CPP #- }
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
#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ <= 704)
  --this makes 7.4 panic! 
  (leftMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (\_ -> 1.0 ::Float )
  (rightMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (\_ -> 1.0 ::Float )
  (resMat :: IODenseMatrix Row Float) <-  generateMutableDenseMatrix SRow (5,5) (\_ -> 1.0 ::Float )
  sgemm NoTranspose NoTranspose 1.0 1.0 leftMat rightMat resMat
  --(MutableDenseMatrix _ _ _ _ buff) <- return resMat 
  theRestMat <- unsafeFreezeDenseMatrix resMat
  putStrLn $ show theRestMat
#endif
--DenseMatrix SRow  5 5 5(fromList [11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,11.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,51.0,251.0,251.0,251.0,251.0,251.0])
-- THAT IS WRONG 
--Need to figure what whats going on here   

  putStrLn $ show res 
  putStrLn "it works!"





-}