{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Numerical.HBLAS.BLAS.FFI.Level2  where

import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex
import Numerical.HBLAS.BLAS.FFI

type GbmvFunFFI sc el =
       CBLAS_ORDERT -> CBLAS_TRANSPOSET -> CInt -> CInt
    -> CInt -> CInt -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()

foreign import ccall unsafe "cblas_sgbmv"
    cblas_sgbmv_unsafe :: GbmvFunFFI Float Float
foreign import ccall unsafe "cblas_dgbmv"
    cblas_dgbmv_unsafe :: GbmvFunFFI Double Double
foreign import ccall unsafe "cblas_cgbmv"
    cblas_cgbmv_unsafe :: GbmvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall unsafe "cblas_zgbmv"
    cblas_zgbmv_unsafe :: GbmvFunFFI (Ptr (Complex Double)) (Complex Double)

foreign import ccall "cblas_sgbmv"
    cblas_sgbmv_safe :: GbmvFunFFI Float Float
foreign import ccall "cblas_dgbmv"
    cblas_dgbmv_safe :: GbmvFunFFI Double Double
foreign import ccall "cblas_cgbmv"
    cblas_cgbmv_safe :: GbmvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall "cblas_zgbmv"
    cblas_zgbmv_safe :: GbmvFunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_sgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   Float alpha,   Float *A,   CInt lda,   Float *X,   CInt incX,   Float beta, Float *Y,   CInt incY);
--void cblas_dgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   Double alpha,   Double *A,   CInt lda,   Double *X,   CInt incX,   Double beta, Double *Y,   CInt incY);
--void cblas_cgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   Float *alpha,   Float *A,   CInt lda,   Float *X,   CInt incX,   Float *beta, Float *Y,   CInt incY);
--void cblas_zgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   Double *alpha,   Double *A,   CInt lda,   Double *X,   CInt incX,   Double *beta, Double *Y,   CInt incY);


{-
matrix vector product for general matrices
 perform one of the matrix-vector operations   y :=
      alpha*A*x + beta*y, or y := alpha*A'*x + beta*y,
-}

type GemvFunFFI sc el =
       CBLAS_ORDERT -> CBLAS_TRANSPOSET -> CInt -> CInt
    -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()

foreign import ccall unsafe "cblas_sgemv"
    cblas_sgemv_unsafe :: GemvFunFFI Float Float
foreign import ccall safe   "cblas_sgemv"
    cblas_sgemv_safe   :: GemvFunFFI Float Float

foreign import ccall unsafe "cblas_dgemv"
    cblas_dgemv_unsafe :: GemvFunFFI Double Double
foreign import ccall safe   "cblas_dgemv"
    cblas_dgemv_safe   :: GemvFunFFI Double Double

foreign import ccall unsafe "cblas_cgemv"
    cblas_cgemv_unsafe :: GemvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe   "cblas_cgemv"
    cblas_cgemv_safe   :: GemvFunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zgemv"
    cblas_zgemv_unsafe :: GemvFunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe   "cblas_zgemv"
    cblas_zgemv_safe   :: GemvFunFFI (Ptr (Complex Double)) (Complex Double)

--void cblas_sgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           Float alpha,   Float  *a,   CInt lda,    Float  *x,   CInt incx,    Float beta,  Float  *y,   CInt incy);
--void cblas_dgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           Double alpha,   Double  *a,   CInt lda,    Double  *x,   CInt incx,    Double beta,  Double  *y,   CInt incy);
--void cblas_cgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           Float *alpha,   Float  *a,   CInt lda,    Float  *x,   CInt incx,    Float *beta,  Float  *y,   CInt incy);
--void cblas_zgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           Double *alpha,   Double  *a,   CInt lda,    Double  *x,   CInt incx,    Double *beta,  Double  *y,   CInt incy);

