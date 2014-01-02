
{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}


module Numerical.OpenBLAS.FFI  where 

import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex 

-- /*Set the number of threads on runtime.*/
foreign import ccall unsafe "cblas.h openblas_set_num_threads" openblas_set_num_threads_ffi :: CInt -> IO ()

foreign import ccall unsafe "cblas.h goto_set_num_threads" goto_set_num_threads_ffi :: CInt -> IO ()




newtype CBLAS_Index = CBIndex CSize 

newtype CBLAS_OrderT = CBOInt CUChar
    deriving (Eq,Show)
data CBLAS_Order = CBLAS_RowMajor | CBLAS_ColMajor 
    deriving (Eq,Show)
encodeOrder CBLAS_RowMajor = CBOInt 101
encodeOrder CBLAS_ColMajor = CBOInt 102 

newtype CBLAS_TransposeT = CBLAS_TransposeT{ unCBLAS_TransposeT :: CUChar } deriving (Eq, Show)

data CBLAS_Tranpose = CBlasNoTranspose | CBlasTranpose | CBlasConjTranspose | CBlasConjNoTranpose 

encodeTranpose  CBlasNoTranspose = CBLAS_TransposeT 111
encodeTranpose  CBlasTranpose = CBLAS_TransposeT 112
encodeTranpose  CBlasConjTranspose = CBLAS_TransposeT 113
encodeTranpose  CBlasConjNoTranpose = CBLAS_TransposeT 114

newtype CBLAS_UploT = CBlasUPLO CUChar
    deriving (Eq,Show)
data CBLAS_Uplo = CBUpper | CBLower
    deriving (Eq,Show)
encodeUPLO CBUpper = CBlasUPLO 121  
encodeUPLO CBLower = CBlasUPLO 122

newtype CBLAS_DiagT = CBLAS_DiagT CUChar 
    deriving (Show,Eq)


data CBlasDiag = CBlasNonUnit   | CBlasUnit 
    deriving (Eq,Show )
    
encodeDiag CBlasNonUnit = CBLAS_DiagT 131
encodeDiag CBlasUnit = CBLAS_DiagT 132

newtype CBLAS_SideT = CBLAS_SideT { unCBLAS_SideT :: CUChar } 
    deriving (Eq, Show)
data CBlasSide = CBlasLeft | CBlasRight 
    deriving (Eq,Show)
encodeSide CBlasLeft = CBLAS_SideT 141
encodeSide CBlasRight = CBLAS_SideT 142


--typedef enum CBLAS_ORDER     {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
--typedef enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, CblasConjNoTrans=114} CBLAS_TRANSPOSE;
--typedef enum CBLAS_UPLO      {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
--typedef enum CBLAS_DIAG      {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
--typedef enum CBLAS_SIDE      {CblasLeft=141, CblasRight=142} CBLAS_SIDE;

--dot products
foreign import ccall unsafe "cblas.h cblas_sdsdot" cblas_sdsdot_ffi :: CInt -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CFloat 
foreign import ccall unsafe "cblas.h cblas_dsdot" cblas_dsdot_ffi :: CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CDouble
foreign import ccall unsafe "cblas.h cblas_sdot" cblas_sdot_ffi :: CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CFloat
foreign import ccall unsafe "cblas.h cblas_ddot" cblas_ddot_ffi :: CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CDouble
--CFloat  cblas_sdsdot(  CInt n,   CFloat alpha,   CFloat *x,   CInt incx,   CFloat *y,   CInt incy);
--CDouble cblas_dsdot (  CInt n,   CFloat *x,   CInt incx,   CFloat *y,   CInt incy);
--CFloat  cblas_sdot(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
--CDouble cblas_ddot(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);

{-
not doing these right now, because requires handling return value as a complex number,
we can only handle pointers to complex numbers right now
-}
--openblas_complex_CFloat  cblas_cdotu(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
--openblas_complex_CFloat  cblas_cdotc(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
--openblas_complex_CDouble cblas_zdotu(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);
--openblas_complex_CDouble cblas_zdotc(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);




--- not sure what to do for these complex 
--void  cblas_cdotu_sub(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy, openblas_complex_CFloat  *ret);
--void  cblas_cdotc_sub(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy, openblas_complex_CFloat  *ret);
--void  cblas_zdotu_sub(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy, openblas_complex_CDouble *ret);
--void  cblas_zdotc_sub(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy, openblas_complex_CDouble *ret);

---- absolute value
foreign import ccall unsafe "cblas.h cblas_sasum" cblas_sasum_ffi :: CInt -> Ptr CFloat -> CInt -> IO CFloat
foreign import ccall unsafe "cblas.h cblas_dasum" cblas_dasum_ffi :: 
    CInt -> Ptr CDouble -> CInt -> IO CDouble
foreign import ccall unsafe "cblas.h cblas_scasum" cblas_casum_ffi :: 
    CInt -> Ptr (Complex CFloat)-> CInt -> IO CFloat
foreign import ccall unsafe "cblas.h cblas_dzasum" cblas_zasum_ffi :: 
    CInt -> Ptr (Complex CDouble) -> CInt -> IO CDouble
--CFloat  cblas_sasum (  CInt n,   CFloat  *x,   CInt incx);
--CDouble cblas_dasum (  CInt n,   CDouble *x,   CInt incx);
--CFloat  cblas_scasum(  CInt n,   CFloat  *x,   CInt incx);
--CDouble cblas_dzasum(  CInt n,   CDouble *x,   CInt incx);



--CFloat  cblas_snrm2 (  CInt N,   CFloat  *X,   CInt incX);
--CDouble cblas_dnrm2 (  CInt N,   CDouble *X,   CInt incX);
--CFloat  cblas_scnrm2(  CInt N,   CFloat  *X,   CInt incX);
--CDouble cblas_dznrm2(  CInt N,   CDouble *X,   CInt incX);

--CBLAS_INDEX cblas_isamax(  CInt n,   CFloat  *x,   CInt incx);
--CBLAS_INDEX cblas_idamax(  CInt n,   CDouble *x,   CInt incx);
--CBLAS_INDEX cblas_icamax(  CInt n,   CFloat  *x,   CInt incx);
--CBLAS_INDEX cblas_izamax(  CInt n,   CDouble *x,   CInt incx);

--void cblas_saxpy(  CInt n,   CFloat alpha,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_daxpy(  CInt n,   CDouble alpha,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);
--void cblas_caxpy(  CInt n,   CFloat *alpha,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_zaxpy(  CInt n,   CDouble *alpha,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);

--void cblas_scopy(  CInt n,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_dcopy(  CInt n,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);
--void cblas_ccopy(  CInt n,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_zcopy(  CInt n,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);

--void cblas_sswap(  CInt n, CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_dswap(  CInt n, CDouble *x,   CInt incx, CDouble *y,   CInt incy);
--void cblas_cswap(  CInt n, CFloat *x,   CInt incx, CFloat *y,   CInt incy);
--void cblas_zswap(  CInt n, CDouble *x,   CInt incx, CDouble *y,   CInt incy);

--void cblas_srot(  CInt N, CFloat *X,   CInt incX, CFloat *Y,   CInt incY,   CFloat c,   CFloat s);
--void cblas_drot(  CInt N, CDouble *X,   CInt incX, CDouble *Y,   CInt incY,   CDouble c,   CDouble  s);

--void cblas_srotg(CFloat *a, CFloat *b, CFloat *c, CFloat *s);
--void cblas_drotg(CDouble *a, CDouble *b, CDouble *c, CDouble *s);

--void cblas_srotm(  CInt N, CFloat *X,   CInt incX, CFloat *Y,   CInt incY,   CFloat *P);
--void cblas_drotm(  CInt N, CDouble *X,   CInt incX, CDouble *Y,   CInt incY,   CDouble *P);

--void cblas_srotmg(CFloat *d1, CFloat *d2, CFloat *b1,   CFloat b2, CFloat *P);
--void cblas_drotmg(CDouble *d1, CDouble *d2, CDouble *b1,   CDouble b2, CDouble *P);

--void cblas_sscal(  CInt N,   CFloat alpha, CFloat *X,   CInt incX);
--void cblas_dscal(  CInt N,   CDouble alpha, CDouble *X,   CInt incX);
--void cblas_cscal(  CInt N,   CFloat *alpha, CFloat *X,   CInt incX);
--void cblas_zscal(  CInt N,   CDouble *alpha, CDouble *X,   CInt incX);
--void cblas_csscal(  CInt N,   CFloat alpha, CFloat *X,   CInt incX);
--void cblas_zdscal(  CInt N,   CDouble alpha, CDouble *X,   CInt incX);

--void cblas_sgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           CFloat alpha,   CFloat  *a,   CInt lda,    CFloat  *x,   CInt incx,    CFloat beta,  CFloat  *y,   CInt incy);
--void cblas_dgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           CDouble alpha,   CDouble  *a,   CInt lda,    CDouble  *x,   CInt incx,    CDouble beta,  CDouble  *y,   CInt incy);
--void cblas_cgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           CFloat *alpha,   CFloat  *a,   CInt lda,    CFloat  *x,   CInt incx,    CFloat *beta,  CFloat  *y,   CInt incy);
--void cblas_zgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
--           CDouble *alpha,   CDouble  *a,   CInt lda,    CDouble  *x,   CInt incx,    CDouble *beta,  CDouble  *y,   CInt incy);



--void cblas_sger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat   alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
--void cblas_dger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble  alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
--void cblas_cgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat  *alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
--void cblas_cgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat  *alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
--void cblas_zgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
--void cblas_zgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);



--void cblas_strsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_dtrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
--void cblas_ctrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_ztrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);



--void cblas_strmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_dtrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
--void cblas_ctrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_ztrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);



--void cblas_ssyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A,   CInt lda);
--void cblas_dsyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *A,   CInt lda);
--void cblas_cher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A,   CInt lda);
--void cblas_zher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *A,   CInt lda);



--void cblas_ssyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,  CInt N,   CFloat alpha,   CFloat *X,
--                  CInt incX,   CFloat *Y,   CInt incY, CFloat *A,   CInt lda);
--void cblas_dsyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,
--                  CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
--void cblas_cher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *X,   CInt incX,
--                  CFloat *Y,   CInt incY, CFloat *A,   CInt lda);
--void cblas_zher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,
--                  CDouble *Y,   CInt incY, CDouble *A,   CInt lda);




--void cblas_sgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   CFloat alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
--void cblas_dgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   CDouble alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);
--void cblas_cgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
--void cblas_zgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
--                   CInt KL,   CInt KU,   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);


--void cblas_ssbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   CFloat alpha,   CFloat *A,
--                   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
--void cblas_dsbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   CDouble alpha,   CDouble *A,
--                   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);



--void cblas_stbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_dtbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
--void cblas_ctbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_ztbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);


--void cblas_stbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_dtbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
--void cblas_ctbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
--void cblas_ztbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);


--void cblas_stpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
--void cblas_dtpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);
--void cblas_ctpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
--void cblas_ztpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);


--void cblas_stpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
--void cblas_dtpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);
--void cblas_ctpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
--void cblas_ztpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);



--void cblas_ssymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *A,
--                   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
--void cblas_dsymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *A,
--                   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);
--void cblas_chemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *A,
--                   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
--void cblas_zhemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *A,
--                   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);


--void cblas_sspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *Ap,
--                   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
--void cblas_dspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *Ap,
--                   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);




--void cblas_sspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *Ap);
--void cblas_dspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *Ap);

--void cblas_chpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A);
--void cblas_zhpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,  CInt incX, CDouble *A);



--void cblas_sspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX,   CFloat *Y,   CInt incY, CFloat *A);
--void cblas_dspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A);
--void cblas_chpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *X,   CInt incX,   CFloat *Y,   CInt incY, CFloat *Ap);
--void cblas_zhpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *Ap);


--void cblas_chbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
--           CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
--void cblas_zhbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
--           CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);


--void cblas_chpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
--           CFloat *alpha,   CFloat *Ap,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
--void cblas_zhpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
--           CDouble *alpha,   CDouble *Ap,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);


{-


-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------------------ BLAS LEVEL 3 ROUTINES ---------------------------
--------------------------------------------------------------------------------
----------------------- Level 3 ops are faster than Levels 1 or 2 -------------- 
--------------------------------------------------------------------------------

type GemmFunFFI scale el = CBLAS_OrderT -> CBLAS_TransposeT ->  CBLAS_TransposeT -> CBLAS_TransposeT->
        CInt -> CInt -> CInt -> scale -> Ptr el  -> CInt -> Ptr el -> CInt -> Ptr el -> IO ()

-- matrix mult!
foreign import ccall unsafe "cblas.h cblas_sgemm" 
    cblas_sgemm_ffi :: GemmFunFFI Float Float

foreign import ccall unsafe "cblas.h cblas_dgemm" 
    cblas_dgemm_ffi :: GemmFunFFI Double Double

foreign import ccall unsafe "cblas.h cblas_cgemm" 
    cblas_cgemm_ffi :: GemmFunFFI (Ptr(Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas.h cblas_zgemm" 
    cblas_zgemm_ffi :: GemmFunFFI (Ptr (Complex Double)) (Complex Double)

-- safe ffi variant for large inputs
foreign import ccall "cblas.h cblas_sgemm" 
    cblas_sgemm_ffi_safe :: GemmFunFFI Float Float

foreign import ccall "cblas.h cblas_dgemm" 
    cblas_dgemm_ffi_safe :: GemmFunFFI Double Double

foreign import ccall "cblas.h cblas_cgemm" 
    cblas_cgemm_ffi_safe :: GemmFunFFI (Ptr(Complex Float)) (Complex Float)

foreign import ccall "cblas.h cblas_zgemm" 
    cblas_zgemm_ffi_safe :: GemmFunFFI (Ptr (Complex Double)) (Complex Double)

-----------------------------------------
----- Matrix mult for Symmetric Matrices
-----------------------------------------


type SymmFunFFI scale el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_UploT ->
     CInt->CInt ->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()

foreign import ccall unsafe "cblas.h cblas_ssymm" 
    cblas_ssymm_ffi :: SymmFunFFI Float Float 

foreign import ccall unsafe "cblas.h cblas_dsymm" 
    cblas_dsymm_ffi :: SymmFunFFI Double Double 

foreign import ccall unsafe "cblas.h cblas_csymm" 
    cblas_csymm_ffi :: SymmFunFFI (Ptr (Complex Float )) (Complex Float)

foreign import ccall unsafe "cblas.h cblas_zsymm" 
    cblas_zsymm_ffi :: SymmFunFFI (Ptr (Complex Double)) (Complex Double)


foreign import ccall  "cblas.h cblas_ssymm" 
    cblas_ssymm_ffi_safe :: SymmFunFFI Float Float 

foreign import ccall  "cblas.h cblas_dsymm" 
    cblas_dsymm_ffi_safe :: SymmFunFFI Double Double 

foreign import ccall  "cblas.h cblas_csymm" 
    cblas_csymm_ffi_safe :: SymmFunFFI (Ptr (Complex Float )) (Complex Float)

foreign import ccall  "cblas.h cblas_zsymm" 
    cblas_zsymm_ffi_safe :: SymmFunFFI (Ptr (Complex Double)) (Complex Double)

 
-----------------------------------
---| symmetric rank k  matrix update, C := alpha*A*A' + beta*C
--- or C = alpha*A'*A + beta*C 
------------------------------------
type SyrkFunFFI scale el = CBLAS_OrderT -> CBLAS_UploT -> CBLAS_TransposeT ->
     CInt->CInt ->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt ->scale -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas.h cblas_ssyrk" 
    cblas_ssyrk_ffi :: SyrkFunFFI Float Float 
foreign import ccall unsafe "cblas.h cblas_dsyrk" 
    cblas_dsyrk_ffi :: SyrkFunFFI Double Double
foreign import ccall unsafe "cblas.h cblas_csyrk" 
    cblas_csyrk_ffi :: SyrkFunFFI (Ptr(Complex Float)) (Complex Float)
foreign import ccall unsafe "cblas.h cblas_zsyrk" 
    cblas_zsyrk_ffi :: SyrkFunFFI (Ptr(Complex Double)) (Complex Double)

----------------------
-----| Symmetric Rank 2k matrix update, C= alpha* A*B' + alpha* B*A' + beta * C
----- or C= alpha* A'*B + alpha* B'*A + beta * C
-------------------


type Syr2kFunFFI scale el = CBLAS_OrderT -> CBLAS_UploT -> CBLAS_TransposeT ->
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> scale ->Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas.h cblas_ssyr2k" 
    cblas_ssyr2k_ffi :: Syr2kFunFFI Float Float 
foreign import ccall unsafe "cblas.h cblas_dsyr2k" 
    cblas_dsyr2k_ffi :: Syr2kFunFFI Double Double
foreign  import ccall unsafe "cblas.h cblas_csyr2k" 
    cblas_csyr2k_ffi :: Syr2kFunFFI (Ptr (Complex Float)) Float  
foreign  import ccall unsafe "cblas.h cblas_zsyr2k" 
    cblas_zsyr2k_ffi :: Syr2kFunFFI (Ptr (Complex Double)) Double 



-------------------------------
-------- matrix matrix product for triangular matrices
------------------------------
type TrmmFunFFI scale el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_UploT -> CBLAS_TransposeT -> CBLAS_DiagT -> 
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign  import ccall unsafe "cblas.h cblas_strmm" 
    cblas_strmm_ffi :: TrmmFunFFI Float Float 
foreign  import ccall unsafe "cblas.h cblas_dtrmm" 
    cblas_dtrmm_ffi :: TrmmFunFFI Double Double 
foreign  import ccall unsafe "cblas.h cblas_ctrmm" 
    cblas_ctrmm_ffi :: TrmmFunFFI (Ptr (Complex Float )) (Complex Float) 
foreign  import ccall unsafe "cblas.h cblas_ztrmm" 
    cblas_ztrmm_ffi :: TrmmFunFFI (Ptr (Complex Double )) (Complex Double) 

--void cblas_strmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
--void cblas_dtrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);
--void cblas_ctrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat *alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
--void cblas_ztrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble *alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);

-- triangular solver 
type TrsmFunFFI scale el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_UploT -> CBLAS_TransposeT -> CBLAS_DiagT -> 
     CInt->CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign  import ccall unsafe "cblas.h cblas_strsm" 
    cblas_strsm_ffi :: TrmmFunFFI Float Float 
foreign  import ccall unsafe "cblas.h cblas_dtrsm" 
    cblas_dtrsm_ffi :: TrmmFunFFI Double Double 
foreign  import ccall unsafe "cblas.h cblas_ctrsm" 
    cblas_ctrsm_ffi :: TrmmFunFFI (Ptr (Complex Float )) (Complex Float) 
foreign  import ccall unsafe "cblas.h cblas_ztrsm" 
    cblas_ztrsm_ffi :: TrmmFunFFI (Ptr (Complex Double )) (Complex Double) 

--void cblas_strsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
--void cblas_dtrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);
--void cblas_ctrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat *alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
--void cblas_ztrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
--                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble *alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);


type HemmFunFFI  el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_UploT ->
     CInt->CInt -> Ptr el -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas.h cblas_chemm" 
    cblas_chemm_ffi :: HemmFunFFI (Complex Float) 
foreign  import ccall unsafe "cblas.h cblas_zhemm" 
    cblas_zhemm_ffi :: HemmFunFFI  (Complex Double) 

--void cblas_chemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
--                   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat *beta, CFloat *C,   CInt ldc);
--void cblas_zhemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
--                   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble *beta, CDouble *C,   CInt ldc);

type HerkFun scale el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_TransposeT ->
     CInt->CInt ->  scale -> Ptr el -> CInt -> Ptr el -> CInt ->scale ->Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas.h cblas_cherk" 
    cblas_cherk_ffi :: HerkFun  Float  (Complex Float) 
foreign  import ccall unsafe "cblas.h cblas_zherk" 
    cblas_zherk_ffi :: HerkFun  Double  (Complex Double) 
--void cblas_cherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                   CFloat alpha,   CFloat *A,   CInt lda,   CFloat beta, CFloat *C,   CInt ldc);
--void cblas_zherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                   CDouble alpha,   CDouble *A,   CInt lda,   CDouble beta, CDouble *C,   CInt ldc);

type Her2kFunFFI scale el = CBLAS_OrderT -> CBLAS_SideT -> CBLAS_TransposeT ->
     CInt->CInt -> Ptr el  -> Ptr el -> CInt -> Ptr el -> CInt ->scale ->Ptr el -> CInt -> IO ()

foreign  import ccall unsafe "cblas.h cblas_cher2k" 
    cblas_cher2k_ffi :: Her2kFunFFI  Float  (Complex Float) 
foreign  import ccall unsafe "cblas.h cblas_zher2k" 
    cblas_zher2k_ffi :: Her2kFunFFI  Double  (Complex Double)
--void cblas_cher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                    CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat beta, CFloat *C,   CInt ldc);
--void cblas_zher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
--                    CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble beta, CDouble *C,   CInt ldc);

----void cblas_xerbla(CInt p, char *rout, char *form, ...);
