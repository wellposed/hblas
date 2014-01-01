module Numerical.OpenBLAS.MatrixTypes where

{-| PSA, the matrix data types used in the hOpenBLAS binding
should not be regarded as being general purpose matrices.

They are designed to exactly express only the matrices which are 
valid inputs for BLAS. When applicable, such matrices should be easily mapped 
to and from other matrix libraries. 
-}    

