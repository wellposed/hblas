{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Numerical.HBLAS.BLAS.FFI.Level1  where

import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex

type AsumFunFFI el res = CInt -> Ptr el -> CInt -> IO res
-- Computes the sum of magnitudes of the vector elements.
foreign import ccall unsafe "cblas_sasum" cblas_sasum_unsafe::
  AsumFunFFI Float Float
-- CInt -> Ptr Float -> CInt -> IO Float
foreign import ccall unsafe "cblas_dasum" cblas_dasum_unsafe ::
  AsumFunFFI Double Double
-- CInt -> Ptr Double -> CInt -> IO Double
foreign import ccall unsafe "cblas_scasum" cblas_scasum_unsafe ::
  AsumFunFFI (Complex Float) Float
-- CInt -> Ptr (Complex Float)-> CInt -> IO Float
foreign import ccall unsafe "cblas_dzasum" cblas_dzasum_unsafe ::
  AsumFunFFI (Complex Double) Double

foreign import ccall "cblas_sasum" cblas_sasum_safe::
  AsumFunFFI Float Float
foreign import ccall "cblas_dasum" cblas_dasum_safe ::
  AsumFunFFI Double Double
foreign import ccall "cblas_scasum" cblas_scasum_safe ::
  AsumFunFFI (Complex Float) Float
foreign import ccall "cblas_dzasum" cblas_dzasum_safe ::
  AsumFunFFI (Complex Double) Double
-- CInt -> Ptr (Complex Double) -> CInt -> IO Double
--Float  cblas_sasum (  CInt n,   Float  *x,   CInt incx);
--Double cblas_dasum (  CInt n,   Double *x,   CInt incx);
--Float  cblas_scasum(  CInt n,   Float  *x,   CInt incx);
--Double cblas_dzasum(  CInt n,   Double *x,   CInt incx);
