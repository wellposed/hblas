Warning: These BLAS routines are written merely for testing / correctness ,
They are in no way intending for serious use in any performance sensitive regime.

Naively written BLAS routines should still be correct and have good numerical stability properties,
but they will easily be 1,000x slower than the serious blas routines.

for reference, 1000x is roughy the difference between 3 seconds and 1 hour


\begin{code}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP  #-}
{-#  LANGUAGE GADTs,ScopedTypeVariables, PolyKinds,FlexibleInstances,DeriveDataTypeable  #-}



module Numerical.HBLAS.SlowCorrectReference.BLAS where 

import Numerical.HBLAS.MatrixTypes 

\end{code}


