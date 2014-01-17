

module Numerical.Lapack.FFI where


--simple drivers for linear equation solvers
--SGESV   CGESV   DGESV   ZGESV


--lapack_int LAPACKE_sgesv( int matrix_order, lapack_int n, lapack_int nrhs,
--                          float* a, lapack_int lda, lapack_int* ipiv, float* b,
--                          lapack_int ldb );
--lapack_int LAPACKE_dgesv( int matrix_order, lapack_int n, lapack_int nrhs,
--                          double* a, lapack_int lda, lapack_int* ipiv,
--                          double* b, lapack_int ldb );
--lapack_int LAPACKE_cgesv( int matrix_order, lapack_int n, lapack_int nrhs,
--                          lapack_complex_float* a, lapack_int lda,
--                          lapack_int* ipiv, lapack_complex_float* b,
--                          lapack_int ldb );
--lapack_int LAPACKE_zgesv( int matrix_order, lapack_int n, lapack_int nrhs,
--                          lapack_complex_double* a, lapack_int lda,
--                          lapack_int* ipiv, lapack_complex_double* b,
--                          lapack_int ldb );



{-


-}

--foreign import  "LAPACKE_sgesv"
              
--foreign import  "LAPACKE_dgesv"
              
              
--foreign import  "LAPACKE_cgesv"
                            
--foreign import  " LAPACKE_zgesv"

