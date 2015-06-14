{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Numerical.HBLAS.BLAS.FFI.Level1  where

import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex

type AsumFunFFI el res = CInt -> Ptr el -> CInt -> IO res
foreign import ccall unsafe "cblas_sasum" cblas_sasum_unsafe::
  AsumFunFFI Float Float
foreign import ccall unsafe "cblas_dasum" cblas_dasum_unsafe::
  AsumFunFFI Double Double
foreign import ccall unsafe "cblas_scasum" cblas_scasum_unsafe::
  AsumFunFFI (Complex Float) Float
foreign import ccall unsafe "cblas_dzasum" cblas_dzasum_unsafe::
  AsumFunFFI (Complex Double) Double

foreign import ccall "cblas_sasum" cblas_sasum_safe::
  AsumFunFFI Float Float
foreign import ccall "cblas_dasum" cblas_dasum_safe ::
  AsumFunFFI Double Double
foreign import ccall "cblas_scasum" cblas_scasum_safe ::
  AsumFunFFI (Complex Float) Float
foreign import ccall "cblas_dzasum" cblas_dzasum_safe ::
  AsumFunFFI (Complex Double) Double
--Float  cblas_sasum (  CInt n,   Float  *x,   CInt incx);
--Double cblas_dasum (  CInt n,   Double *x,   CInt incx);
--Float  cblas_scasum(  CInt n,   Float  *x,   CInt incx);
--Double cblas_dzasum(  CInt n,   Double *x,   CInt incx);

type AxpyFunFFI scale el = CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_saxpy" cblas_saxpy_unsafe::
  AxpyFunFFI Float Float
foreign import ccall unsafe "cblas_daxpy" cblas_daxpy_unsafe::
  AxpyFunFFI Double Double
foreign import ccall unsafe "cblas_caxpy" cblas_caxpy_unsafe::
  AxpyFunFFI (Ptr (Complex Float )) (Complex Float )
foreign import ccall unsafe "cblas_zaxpy" cblas_zaxpy_unsafe::
  AxpyFunFFI (Ptr (Complex Double)) (Complex Double)

foreign import ccall "cblas_saxpy" cblas_saxpy_safe::
  AxpyFunFFI Float Float
foreign import ccall "cblas_daxpy" cblas_daxpy_safe::
  AxpyFunFFI Double Double
foreign import ccall "cblas_caxpy" cblas_caxpy_safe::
  AxpyFunFFI (Ptr (Complex Float )) (Complex Float )
foreign import ccall "cblas_zaxpy" cblas_zaxpy_safe::
  AxpyFunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_saxpy(  CInt n,   Float alpha,   Float *x,   CInt incx, Float *y,   CInt incy);
--void cblas_daxpy(  CInt n,   Double alpha,   Double *x,   CInt incx, Double *y,   CInt incy);
--void cblas_caxpy(  CInt n,   Float *alpha,   Float *x,   CInt incx, Float *y,   CInt incy);
--void cblas_zaxpy(  CInt n,   Double *alpha,   Double *x,   CInt incx, Double *y,   CInt incy);

type CopyFunFFI el = CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_scopy" cblas_scopy_unsafe ::
    CopyFunFFI Float
foreign import ccall unsafe "cblas_dcopy" cblas_dcopy_unsafe ::
    CopyFunFFI Double
foreign import ccall unsafe "cblas_ccopy" cblas_ccopy_unsafe ::
    CopyFunFFI (Complex Float)
foreign import ccall unsafe "cblas_zcopy" cblas_zcopy_unsafe ::
    CopyFunFFI (Complex Double)

foreign import ccall "cblas_scopy" cblas_scopy_safe ::
    CopyFunFFI Float
foreign import ccall "cblas_dcopy" cblas_dcopy_safe ::
    CopyFunFFI Double
foreign import ccall "cblas_ccopy" cblas_ccopy_safe ::
    CopyFunFFI (Complex Float)
foreign import ccall "cblas_zcopy" cblas_zcopy_safe ::
    CopyFunFFI (Complex Double)
--void cblas_scopy(  CInt n,   Float *x,   CInt incx, Float *y,   CInt incy);
--void cblas_dcopy(  CInt n,   Double *x,   CInt incx, Double *y,   CInt incy);
--void cblas_ccopy(  CInt n,   Float *x,   CInt incx, Float *y,   CInt incy);
--void cblas_zcopy(  CInt n,   Double *x,   CInt incx, Double *y,   CInt incy);

--dot products
type NoScalarDotFunFFI el res = CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO res
type ScalarDotFunFFI el res = CInt -> el -> Ptr el -> CInt -> Ptr el -> CInt -> IO res
foreign import ccall "cblas_sdsdot" cblas_sdsdot_safe :: ScalarDotFunFFI Float Float
foreign import ccall "cblas_dsdot" cblas_dsdot_safe :: NoScalarDotFunFFI Float Double
foreign import ccall "cblas_sdot" cblas_sdot_safe :: NoScalarDotFunFFI Float Float
foreign import ccall "cblas_ddot" cblas_ddot_safe :: NoScalarDotFunFFI Double Double

foreign import ccall unsafe "cblas_sdsdot" cblas_sdsdot_unsafe :: ScalarDotFunFFI Float Float
foreign import ccall unsafe "cblas_dsdot" cblas_dsdot_unsafe :: NoScalarDotFunFFI Float Double
foreign import ccall unsafe "cblas_sdot" cblas_sdot_unsafe :: NoScalarDotFunFFI Float Float
foreign import ccall unsafe "cblas_ddot" cblas_ddot_unsafe :: NoScalarDotFunFFI Double Double
--Float  cblas_sdsdot(  CInt n,   Float alpha,   Float *x,   CInt incx,   Float *y,   CInt incy);
--Double cblas_dsdot (  CInt n,   Float *x,   CInt incx,   Float *y,   CInt incy);
--Float  cblas_sdot(  CInt n,   Float  *x,   CInt incx,   Float  *y,   CInt incy);
--Double cblas_ddot(  CInt n,   Double *x,   CInt incx,   Double *y,   CInt incy);

