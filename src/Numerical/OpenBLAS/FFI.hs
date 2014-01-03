{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numerical.OpenBLAS.FFI where
import Foreign.Ptr
import Foreign.C.Types
import Data.Complex
foreign import ccall unsafe "tests/cblas.h openblas_set_num_threads" openblas_set_num_threads :: CInt ->
                                                                                                 IO ()
foreign import ccall unsafe "tests/cblas.h goto_set_num_threads" goto_set_num_threads :: CInt ->
                                                                                         IO ()
foreign import ccall unsafe "tests/cblas.h openblas_get_config" openblas_get_config :: IO CChar
foreign import ccall unsafe "tests/cblas.h openblas_get_parallel" openblas_get_parallel :: IO CInt
newtype CBLAS_ORDERT
  = CBLAS_ORDERT {unCBLAS_ORDERT :: CUChar}
    deriving (Eq, Show)
data CBLAS_ORDER
    = CblasRowMajor | CblasColMajor
    deriving (Eq, Show)
encodeOrder :: CBLAS_ORDER -> CBLAS_ORDERT
encodeOrder (CblasRowMajor) = CBLAS_ORDERT 101
encodeOrder (CblasColMajor) = CBLAS_ORDERT 102
newtype CBLAS_TRANSPOSET
  = CBLAS_TRANSPOSET {unCBLAS_TRANSPOSET :: CUChar}
    deriving (Eq, Show)
data CBLAS_TRANSPOSE
    = CblasNoTrans | CblasTrans | CblasConjTrans | CblasConjNoTrans
    deriving (Eq, Show)
encodeTranspose :: CBLAS_TRANSPOSE -> CBLAS_TRANSPOSET
encodeTranspose (CblasNoTrans) = CBLAS_TRANSPOSET 111
encodeTranspose (CblasTrans) = CBLAS_TRANSPOSET 112
encodeTranspose (CblasConjTrans) = CBLAS_TRANSPOSET 113
encodeTranspose (CblasConjNoTrans) = CBLAS_TRANSPOSET 114
newtype CBLAS_UPLOT
  = CBLAS_UPLOT {unCBLAS_UPLOT :: CUChar}
    deriving (Eq, Show)
data CBLAS_UPLO = CblasUpper | CblasLower deriving (Eq, Show)
encodeUplo :: CBLAS_UPLO -> CBLAS_UPLOT
encodeUplo (CblasUpper) = CBLAS_UPLOT 121
encodeUplo (CblasLower) = CBLAS_UPLOT 122
newtype CBLAS_DIAGT
  = CBLAS_DIAGT {unCBLAS_DIAGT :: CUChar}
    deriving (Eq, Show)
data CBLAS_DIAG = CblasNonUnit | CblasUnit deriving (Eq, Show)
encodeDiag :: CBLAS_DIAG -> CBLAS_DIAGT
encodeDiag (CblasNonUnit) = CBLAS_DIAGT 131
encodeDiag (CblasUnit) = CBLAS_DIAGT 132
newtype CBLAS_SIDET
  = CBLAS_SIDET {unCBLAS_SIDET :: CUChar}
    deriving (Eq, Show)
data CBLAS_SIDE = CblasLeft | CblasRight deriving (Eq, Show)
encodeSide :: CBLAS_SIDE -> CBLAS_SIDET
encodeSide (CblasLeft) = CBLAS_SIDET 141
encodeSide (CblasRight) = CBLAS_SIDET 142
foreign import ccall unsafe "tests/cblas.h cblas_sdsdot" cblas_sdsdot :: CInt ->
                                                                         CFloat ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_dsdot" cblas_dsdot :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_sdot" cblas_sdot :: CInt ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat -> CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_ddot" cblas_ddot :: CInt ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble ->
                                                                     CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_cdotu" cblas_cdotu :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       IO (Ptr (Complex Float))
foreign import ccall unsafe "tests/cblas.h cblas_cdotc" cblas_cdotc :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       IO (Ptr (Complex Float))
foreign import ccall unsafe "tests/cblas.h cblas_zdotu" cblas_zdotu :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       IO (Ptr (Complex Double))
foreign import ccall unsafe "tests/cblas.h cblas_zdotc" cblas_zdotc :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       IO (Ptr (Complex Double))
foreign import ccall unsafe "tests/cblas.h cblas_cdotu_sub" cblas_cdotu_sub :: CInt ->
                                                                               Ptr CFloat ->
                                                                               CInt ->
                                                                               Ptr CFloat ->
                                                                               CInt ->
                                                                               Ptr (Ptr (Complex Float)) ->
                                                                               IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cdotc_sub" cblas_cdotc_sub :: CInt ->
                                                                               Ptr CFloat ->
                                                                               CInt ->
                                                                               Ptr CFloat ->
                                                                               CInt ->
                                                                               Ptr (Ptr (Complex Float)) ->
                                                                               IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zdotu_sub" cblas_zdotu_sub :: CInt ->
                                                                               Ptr CDouble ->
                                                                               CInt ->
                                                                               Ptr CDouble ->
                                                                               CInt ->
                                                                               Ptr (Ptr (Complex Double)) ->
                                                                               IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zdotc_sub" cblas_zdotc_sub :: CInt ->
                                                                               Ptr CDouble ->
                                                                               CInt ->
                                                                               Ptr CDouble ->
                                                                               CInt ->
                                                                               Ptr (Ptr (Complex Double)) ->
                                                                               IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sasum" cblas_sasum :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_dasum" cblas_dasum :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_scasum" cblas_scasum :: CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_dzasum" cblas_dzasum :: CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_snrm2" cblas_snrm2 :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_dnrm2" cblas_dnrm2 :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_scnrm2" cblas_scnrm2 :: CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt -> IO CFloat
foreign import ccall unsafe "tests/cblas.h cblas_dznrm2" cblas_dznrm2 :: CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO CDouble
foreign import ccall unsafe "tests/cblas.h cblas_isamax" cblas_isamax :: CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt -> IO CUInt
foreign import ccall unsafe "tests/cblas.h cblas_idamax" cblas_idamax :: CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO CUInt
foreign import ccall unsafe "tests/cblas.h cblas_icamax" cblas_icamax :: CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt -> IO CUInt
foreign import ccall unsafe "tests/cblas.h cblas_izamax" cblas_izamax :: CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO CUInt
foreign import ccall unsafe "tests/cblas.h cblas_saxpy" cblas_saxpy :: CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_daxpy" cblas_daxpy :: CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_caxpy" cblas_caxpy :: CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zaxpy" cblas_zaxpy :: CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_scopy" cblas_scopy :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dcopy" cblas_dcopy :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ccopy" cblas_ccopy :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zcopy" cblas_zcopy :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sswap" cblas_sswap :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dswap" cblas_dswap :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cswap" cblas_cswap :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zswap" cblas_zswap :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_srot" cblas_srot :: CInt ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     CFloat -> CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_drot" cblas_drot :: CInt ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     CDouble -> CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_srotg" cblas_srotg :: Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_drotg" cblas_drotg :: Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_srotm" cblas_srotm :: CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_drotm" cblas_drotm :: CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_srotmg" cblas_srotmg :: Ptr CFloat ->
                                                                         Ptr CFloat ->
                                                                         Ptr CFloat ->
                                                                         CFloat ->
                                                                         Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_drotmg" cblas_drotmg :: Ptr CDouble ->
                                                                         Ptr CDouble ->
                                                                         Ptr CDouble ->
                                                                         CDouble ->
                                                                         Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sscal" cblas_sscal :: CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dscal" cblas_dscal :: CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cscal" cblas_cscal :: CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zscal" cblas_zscal :: CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_csscal" cblas_csscal :: CInt ->
                                                                         CFloat ->
                                                                         Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zdscal" cblas_zdscal :: CInt ->
                                                                         CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sgemv" cblas_sgemv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dgemv" cblas_dgemv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cgemv" cblas_cgemv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zgemv" cblas_zgemv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sger" cblas_sger :: CBLAS_ORDERT ->
                                                                     CInt ->
                                                                     CInt ->
                                                                     CFloat ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dger" cblas_dger :: CBLAS_ORDERT ->
                                                                     CInt ->
                                                                     CInt ->
                                                                     CDouble ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cgeru" cblas_cgeru :: CBLAS_ORDERT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cgerc" cblas_cgerc :: CBLAS_ORDERT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zgeru" cblas_zgeru :: CBLAS_ORDERT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zgerc" cblas_zgerc :: CBLAS_ORDERT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_strsv" cblas_strsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtrsv" cblas_dtrsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctrsv" cblas_ctrsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztrsv" cblas_ztrsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_strmv" cblas_strmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtrmv" cblas_dtrmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctrmv" cblas_ctrmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztrmv" cblas_ztrmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssyr" cblas_ssyr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CFloat ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsyr" cblas_dsyr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CDouble ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cher" cblas_cher :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CFloat ->
                                                                     Ptr CFloat ->
                                                                     CInt ->
                                                                     Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zher" cblas_zher :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CDouble ->
                                                                     Ptr CDouble ->
                                                                     CInt ->
                                                                     Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssyr2" cblas_ssyr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsyr2" cblas_dsyr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cher2" cblas_cher2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zher2" cblas_zher2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sgbmv" cblas_sgbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dgbmv" cblas_dgbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cgbmv" cblas_cgbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zgbmv" cblas_zgbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssbmv" cblas_ssbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsbmv" cblas_dsbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_stbmv" cblas_stbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtbmv" cblas_dtbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctbmv" cblas_ctbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztbmv" cblas_ztbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_stbsv" cblas_stbsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtbsv" cblas_dtbsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctbsv" cblas_ctbsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztbsv" cblas_ztbsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_stpmv" cblas_stpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtpmv" cblas_dtpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctpmv" cblas_ctpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztpmv" cblas_ztpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_stpsv" cblas_stpsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtpsv" cblas_dtpsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctpsv" cblas_ctpsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztpsv" cblas_ztpsv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssymv" cblas_ssymv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsymv" cblas_dsymv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chemv" cblas_chemv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhemv" cblas_zhemv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sspmv" cblas_sspmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dspmv" cblas_dspmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sspr" cblas_sspr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CFloat ->
                                                                     Ptr CFloat ->
                                                                     CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dspr" cblas_dspr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CDouble ->
                                                                     Ptr CDouble ->
                                                                     CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chpr" cblas_chpr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CFloat ->
                                                                     Ptr CFloat ->
                                                                     CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhpr" cblas_zhpr :: CBLAS_ORDERT ->
                                                                     CBLAS_UPLOT ->
                                                                     CInt ->
                                                                     CDouble ->
                                                                     Ptr CDouble ->
                                                                     CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sspr2" cblas_sspr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dspr2" cblas_dspr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chpr2" cblas_chpr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhpr2" cblas_zhpr2 :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chbmv" cblas_chbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhbmv" cblas_zhbmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chpmv" cblas_chpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhpmv" cblas_zhpmv :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_sgemm" cblas_sgemm :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dgemm" cblas_dgemm :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cgemm" cblas_cgemm :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zgemm" cblas_zgemm :: CBLAS_ORDERT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssymm" cblas_ssymm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsymm" cblas_dsymm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_csymm" cblas_csymm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zsymm" cblas_zsymm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssyrk" cblas_ssyrk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsyrk" cblas_dsyrk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_csyrk" cblas_csyrk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zsyrk" cblas_zsyrk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ssyr2k" cblas_ssyr2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         CFloat ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         CFloat ->
                                                                         Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dsyr2k" cblas_dsyr2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_csyr2k" cblas_csyr2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zsyr2k" cblas_zsyr2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_strmm" cblas_strmm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtrmm" cblas_dtrmm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctrmm" cblas_ctrmm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztrmm" cblas_ztrmm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_strsm" cblas_strsm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_dtrsm" cblas_dtrsm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ctrsm" cblas_ctrsm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_ztrsm" cblas_ztrsm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CBLAS_DIAGT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_chemm" cblas_chemm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       Ptr CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zhemm" cblas_zhemm :: CBLAS_ORDERT ->
                                                                       CBLAS_SIDET ->
                                                                       CBLAS_UPLOT ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       Ptr CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cherk" cblas_cherk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat ->
                                                                       CInt ->
                                                                       CFloat ->
                                                                       Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zherk" cblas_zherk :: CBLAS_ORDERT ->
                                                                       CBLAS_UPLOT ->
                                                                       CBLAS_TRANSPOSET ->
                                                                       CInt ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble ->
                                                                       CInt ->
                                                                       CDouble ->
                                                                       Ptr CDouble -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_cher2k" cblas_cher2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         Ptr CFloat ->
                                                                         CInt ->
                                                                         CFloat ->
                                                                         Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_zher2k" cblas_zher2k :: CBLAS_ORDERT ->
                                                                         CBLAS_UPLOT ->
                                                                         CBLAS_TRANSPOSET ->
                                                                         CInt ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         Ptr CDouble ->
                                                                         CInt ->
                                                                         CDouble ->
                                                                         Ptr CDouble ->
                                                                         CInt -> IO ()
foreign import ccall unsafe "tests/cblas.h cblas_xerbla" cblas_xerbla :: CInt ->
                                                                         Ptr CChar ->
                                                                         Ptr CChar -> IO ()
