{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Numerical.HBLAS.BLAS.FFI.Level3  where

import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex
import Numerical.HBLAS.BLAS.FFI

--------------------------------------------------------------------------------
------------------------------ | BLAS LEVEL 3 ROUTINES
--------------------------------------------------------------------------------
-----------------------  |  Level 3 ops are faster than Levels 1 or 2
--------------------------------------------------------------------------------


--void cblas_sgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_TRANSPOSE TransB, const blasint M, const blasint N, const blasint K,
--         const float alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float beta, float *C, const blasint ldc);

--void cblas_dgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_TRANSPOSE TransB, const blasint M, const blasint N, const blasint K,
--         const double alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double beta, double *C, const blasint ldc);
--void cblas_cgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_TRANSPOSE TransB, const blasint M, const blasint N, const blasint K,
--         const float *alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float *beta, float *C, const blasint ldc);
--void cblas_zgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_TRANSPOSE TransB, const blasint M, const blasint N, const blasint K,
--         const double *alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double *beta, double *C, const blasint ldc);

-- |  Matrix mult for general dense matrices
type GemmFunFFI scale el = CBLAS_ORDERT ->   CBLAS_TRANSPOSET -> CBLAS_TRANSPOSET->
        CInt -> CInt -> CInt -> {- scal A * B -} scale  -> {- Matrix A-} Ptr el  -> CInt -> {- B -}  Ptr el -> CInt->
            scale -> {- C -}  Ptr el -> CInt -> IO ()

{- C := alpha*op( A )*op( B ) + beta*C ,  -}

-- matrix mult!
foreign import ccall unsafe "cblas_sgemm"
    cblas_sgemm_unsafe :: GemmFunFFI Float Float

foreign import ccall unsafe "cblas_dgemm"
    cblas_dgemm_unsafe :: GemmFunFFI Double Double

foreign import ccall unsafe "cblas_cgemm"
    cblas_cgemm_unsafe :: GemmFunFFI (Ptr(Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zgemm"
    cblas_zgemm_unsafe :: GemmFunFFI (Ptr (Complex Double)) (Complex Double)

-- safe ffi variant for large inputs
foreign import ccall "cblas_sgemm"
    cblas_sgemm_safe :: GemmFunFFI Float Float

foreign import ccall "cblas_dgemm"
    cblas_dgemm_safe :: GemmFunFFI Double Double

foreign import ccall "cblas_cgemm"
    cblas_cgemm_safe :: GemmFunFFI (Ptr(Complex Float)) (Complex Float)

foreign import ccall "cblas_zgemm"
    cblas_zgemm_safe :: GemmFunFFI (Ptr (Complex Double)) (Complex Double)

-----------------------------------------
----- |  Matrix mult for Symmetric Matrices
-----------------------------------------


--void cblas_ssymm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side, const enum CBLAS_UPLO Uplo, const blasint M, const blasint N,
--                 const float alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float beta, float *C, const blasint ldc);
--void cblas_dsymm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side, const enum CBLAS_UPLO Uplo, const blasint M, const blasint N,
--                 const double alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double beta, double *C, const blasint ldc);
--void cblas_csymm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side, const enum CBLAS_UPLO Uplo, const blasint M, const blasint N,
--                 const float *alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float *beta, float *C, const blasint ldc);
--void cblas_zsymm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side, const enum CBLAS_UPLO Uplo, const blasint M, const blasint N,
--                 const double *alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double *beta, double *C, const blasint ldc);

type SymmFunFFI scale el = CBLAS_ORDERT -> CBLAS_SIDET -> CBLAS_UPLOT ->
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> scale ->Ptr el -> CInt -> IO ()

foreign import ccall unsafe "cblas_ssymm"
    cblas_ssymm_unsafe :: SymmFunFFI Float Float

foreign import ccall unsafe "cblas_dsymm"
    cblas_dsymm_unsafe :: SymmFunFFI Double Double

foreign import ccall unsafe "cblas_csymm"
    cblas_csymm_unsafe :: SymmFunFFI (Ptr (Complex Float )) (Complex Float)

foreign import ccall unsafe "cblas_zsymm"
    cblas_zsymm_unsafe :: SymmFunFFI (Ptr (Complex Double)) (Complex Double)

-- safe ffi variant,
foreign import ccall  "cblas_ssymm"
    cblas_ssymm_safe :: SymmFunFFI Float Float

foreign import ccall  "cblas_dsymm"
    cblas_dsymm_safe :: SymmFunFFI Double Double

foreign import ccall  "cblas_csymm"
    cblas_csymm_safe :: SymmFunFFI (Ptr (Complex Float )) (Complex Float)

foreign import ccall  "cblas_zsymm"
    cblas_zsymm_safe :: SymmFunFFI (Ptr (Complex Double)) (Complex Double)


-----------------------------------
--- |  symmetric rank k  matrix update, C := alpha*A*A' + beta*C
--- or C = alpha*A'*A + beta*C
------------------------------------


--void cblas_ssyrk(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--         const blasint N, const blasint K, const float alpha, const float *A, const blasint lda, const float beta, float *C, const blasint ldc);
--void cblas_dsyrk(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--         const blasint N, const blasint K, const double alpha, const double *A, const blasint lda, const double beta, double *C, const blasint ldc);
--void cblas_csyrk(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--         const blasint N, const blasint K, const float *alpha, const float *A, const blasint lda, const float *beta, float *C, const blasint ldc);
--void cblas_zsyrk(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--         const blasint N, const blasint K, const double *alpha, const double *A, const blasint lda, const double *beta, double *C, const blasint ldc);

type SyrkFunFFI scale el = CBLAS_ORDERT -> CBLAS_UPLOT -> CBLAS_TRANSPOSET ->
     CInt->CInt  -> scale -> Ptr el -> CInt -> Ptr el -> CInt ->scale -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_ssyrk"
    cblas_ssyrk_unsafe :: SyrkFunFFI Float Float
foreign import ccall unsafe "cblas_dsyrk"
    cblas_dsyrk_unsafe :: SyrkFunFFI Double Double
foreign import ccall unsafe "cblas_csyrk"
    cblas_csyrk_unsafe :: SyrkFunFFI (Ptr(Complex Float)) (Complex Float)
foreign import ccall unsafe "cblas_zsyrk"
    cblas_zsyrk_unsafe :: SyrkFunFFI (Ptr(Complex Double)) (Complex Double)

----------------------
----- | Symmetric Rank 2k matrix update, C= alpha* A*B' + alpha* B*A' + beta * C
----- or C= alpha* A'*B + alpha* B'*A + beta * C
-------------------


--void cblas_ssyr2k(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--          const blasint N, const blasint K, const float alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float beta, float *C, const blasint ldc);
--void cblas_dsyr2k(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--          const blasint N, const blasint K, const double alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double beta, double *C, const blasint ldc);
--void cblas_csyr2k(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
--          const blasint N, const blasint K, const float *alpha, const float *A, const blasint lda, const float *B, const blasint ldb, const float *beta, float *C, const blasint ldc);
--void cblas_zsyr2k(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE Trans,
         --const blasint N, const blasint K, const double *alpha, const double *A, const blasint lda, const double *B, const blasint ldb, const double *beta, double *C, const blasint ldc);

type Syr2kFunFFI scale el = CBLAS_ORDERT -> CBLAS_UPLOT -> CBLAS_TRANSPOSET  ->
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt ->
     scale ->Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas_ssyr2k"
    cblas_ssyr2k_unsafe :: Syr2kFunFFI Float Float
foreign import ccall unsafe "cblas_dsyr2k"
    cblas_dsyr2k_unsafe :: Syr2kFunFFI Double Double
foreign  import ccall unsafe "cblas_csyr2k"
    cblas_csyr2k_unsafe :: Syr2kFunFFI (Ptr (Complex Float)) Float
foreign  import ccall unsafe "cblas_zsyr2k"
    cblas_zsyr2k_unsafe :: Syr2kFunFFI (Ptr (Complex Double)) Double



-------------------------------
--------  |  matrix matrix product for triangular matrices
------------------------------





type TrmmFunFFI scale el = CBLAS_ORDERT -> CBLAS_SIDET -> CBLAS_UPLOT -> CBLAS_TRANSPOSET -> CBLAS_DIAGT ->
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign  import ccall unsafe "cblas_strmm"
    cblas_strmm_unsafe :: TrmmFunFFI Float Float
foreign  import ccall unsafe "cblas_dtrmm"
    cblas_dtrmm_unsafe :: TrmmFunFFI Double Double
foreign  import ccall unsafe "cblas_ctrmm"
    cblas_ctrmm_unsafe :: TrmmFunFFI (Ptr (Complex Float )) (Complex Float)
foreign  import ccall unsafe "cblas_ztrmm"
    cblas_ztrmm_unsafe :: TrmmFunFFI (Ptr (Complex Double )) (Complex Double)

--void cblas_strmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Float alpha,   Float *A,   CInt lda, Float *B,   CInt ldb);
--void cblas_dtrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Double alpha,   Double *A,   CInt lda, Double *B,   CInt ldb);
--void cblas_ctrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Float *alpha,   Float *A,   CInt lda, Float *B,   CInt ldb);
--void cblas_ztrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Double *alpha,   Double *A,   CInt lda, Double *B,   CInt ldb);

------------------------
--  |  triangular solvers
-----------------------


--
--TRSM solves  op(A)*X = alpha*B or  X*op(A) = alpha*B
--op(A) is one of op(A) = A, or op(A) = A', or op(A) = conjg(A').
-- A is a unit, or non-unit, upper or lower triangular matrix
----
type TrsmFunFFI scale el = CBLAS_ORDERT -> CBLAS_SIDET -> CBLAS_UPLOT -> CBLAS_TRANSPOSET -> CBLAS_DIAGT ->
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign  import ccall unsafe "cblas_strsm"
    cblas_strsm_unsafe :: TrmmFunFFI Float Float
foreign  import ccall unsafe "cblas_dtrsm"
    cblas_dtrsm_unsafe :: TrmmFunFFI Double Double
foreign  import ccall unsafe "cblas_ctrsm"
    cblas_ctrsm_unsafe :: TrmmFunFFI (Ptr (Complex Float )) (Complex Float)
foreign  import ccall unsafe "cblas_ztrsm"
    cblas_ztrsm_unsafe :: TrmmFunFFI (Ptr (Complex Double )) (Complex Double)

--void cblas_strsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Float alpha,   Float *A,   CInt lda, Float *B,   CInt ldb);
--void cblas_dtrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Double alpha,   Double *A,   CInt lda, Double *B,   CInt ldb);
--void cblas_ctrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Float *alpha,   Float *A,   CInt lda, Float *B,   CInt ldb);
--void cblas_ztrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   Double *alpha,   Double *A,   CInt lda, Double *B,   CInt ldb);

-------------------------
-- | hermitian matrix mult
------------------------

type HemmFunFFI  el = CBLAS_ORDERT -> CBLAS_SIDET -> CBLAS_UPLOT ->
     CInt -> CInt -> Ptr el -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas_chemm"
    cblas_chemm_unsafe :: HemmFunFFI (Complex Float)
foreign  import ccall unsafe "cblas_zhemm"
    cblas_zhemm_unsafe :: HemmFunFFI  (Complex Double)

foreign  import ccall safe "cblas_chemm"
    cblas_chemm_safe :: HemmFunFFI (Complex Float)
foreign  import ccall safe "cblas_zhemm"
    cblas_zhemm_safe :: HemmFunFFI  (Complex Double)

--void cblas_chemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
--                   Float *alpha,   Float *A,   CInt lda,   Float *B,   CInt ldb,   Float *beta, Float *C,   CInt ldc);
--void cblas_zhemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
--                   Double *alpha,   Double *A,   CInt lda,   Double *B,   CInt ldb,   Double *beta, Double *C,   CInt ldc);

type HerkFunFFI scale el = CBLAS_ORDERT -> CBLAS_UPLOT -> CBLAS_TRANSPOSET ->
     CInt -> CInt -> scale -> Ptr el -> CInt -> scale -> Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas_cherk"
    cblas_cherk_unsafe :: HerkFunFFI Float (Complex Float)
foreign  import ccall unsafe "cblas_zherk"
    cblas_zherk_unsafe :: HerkFunFFI Double (Complex Double)

foreign  import ccall safe "cblas_cherk"
    cblas_cherk_safe :: HerkFunFFI Float (Complex Float)
foreign  import ccall safe "cblas_zherk"
    cblas_zherk_safe :: HerkFunFFI Double (Complex Double)
--void cblas_cherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                   Float alpha,   Float *A,   CInt lda,   Float beta, Float *C,   CInt ldc);
--void cblas_zherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                   Double alpha,   Double *A,   CInt lda,   Double beta, Double *C,   CInt ldc);

type Her2kFunFFI scale el = CBLAS_ORDERT -> CBLAS_SIDET -> CBLAS_TRANSPOSET ->
     CInt -> CInt -> Ptr el  -> Ptr el -> CInt -> Ptr el -> CInt ->scale ->Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas_cher2k"
    cblas_cher2k_unsafe :: Her2kFunFFI  Float  (Complex Float)
foreign  import ccall unsafe "cblas_zher2k"
    cblas_zher2k_unsafe :: Her2kFunFFI  Double  (Complex Double)


--void cblas_cher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                    Float *alpha,   Float *A,   CInt lda,   Float *B,   CInt ldb,   Float beta, Float *C,   CInt ldc);
--void cblas_zher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                    Double *alpha,   Double *A,   CInt lda,   Double *B,   CInt ldb,   Double beta, Double *C,   CInt ldc);

----void cblas_xerbla(CInt p, char *rout, char *form, ...);
