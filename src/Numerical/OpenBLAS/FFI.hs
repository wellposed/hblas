{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module Numerical.OpenBLAS.FFI  where 

import Foreign.Ptr
import Foreign()
import Foreign.C.Types

--/*Set the number of threads on runtime.*/
foreign import ccall unsafe "openblas_set_num_threads" openblas_set_num_threads_unsafe :: CInt -> IO ()

foreign import ccall unsafe "goto_set_num_threads" goto_set_num_threads_unsafe :: CInt -> IO ()


#define OPENBLAS_OPENMP 2 


/*
 * Since all of GotoBlas was written without const,
 * we disable it at build time.
 */



--#define CBLAS_INDEX size_t
newtype CBLAS_Index = CBIndex CSize 

newtype CBLAS_ORDER_INT = CBOInt CInt 
    deriving (Eq,Show)
data CBLAS_Order = CBLAS_RowMajor | CBLAS_ColMajor 
    deriving (Eq,Show)
encodeOrder CBLAS_RowMajor = CBOInt 101
encodeOrder CBLAS_ColMajor = CBOInt 102 

newtype CBLAS_TransposeInt = CBLAS_TransposeInt { unCBLAS_TransposeInt :: CInt } deriving (Eq, Show)
data CBLAS_Tranpose = CBlasNoTransPose | CBlasTranpose | CBlasConjTranspose | CBlasConjNoTranpose 

encodeTranpose  CBlasNoTransPose = CBLAS_TransposeInt 111
encodeTranpose  CBlasTranpose = CBLAS_TransposeInt 112
encodeTranpose  CBlasConjTranspose = CBLAS_TransposeInt 113
encodeTranpose  CBlasConjNoTranpose = CBLAS_TransposeInt 114


--typedef enum CBLAS_ORDER     {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
--typedef enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, CblasConjNoTrans=114} CBLAS_TRANSPOSE;
typedef enum CBLAS_UPLO      {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
typedef enum CBLAS_DIAG      {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
typedef enum CBLAS_SIDE      {CblasLeft=141, CblasRight=142} CBLAS_SIDE;

CFloat  cblas_sdsdot(  CInt n,   CFloat alpha,   CFloat *x,   CInt incx,   CFloat *y,   CInt incy);
CDouble cblas_dsdot (  CInt n,   CFloat *x,   CInt incx,   CFloat *y,   CInt incy);
CFloat  cblas_sdot(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
CDouble cblas_ddot(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);

--openblas_complex_CFloat  cblas_cdotu(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
--openblas_complex_CFloat  cblas_cdotc(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy);
--openblas_complex_CDouble cblas_zdotu(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);
--openblas_complex_CDouble cblas_zdotc(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy);

void  cblas_cdotu_sub(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy, openblas_complex_CFloat  *ret);
void  cblas_cdotc_sub(  CInt n,   CFloat  *x,   CInt incx,   CFloat  *y,   CInt incy, openblas_complex_CFloat  *ret);
void  cblas_zdotu_sub(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy, openblas_complex_CDouble *ret);
void  cblas_zdotc_sub(  CInt n,   CDouble *x,   CInt incx,   CDouble *y,   CInt incy, openblas_complex_CDouble *ret);

CFloat  cblas_sasum (  CInt n,   CFloat  *x,   CInt incx);
CDouble cblas_dasum (  CInt n,   CDouble *x,   CInt incx);
CFloat  cblas_scasum(  CInt n,   CFloat  *x,   CInt incx);
CDouble cblas_dzasum(  CInt n,   CDouble *x,   CInt incx);

CFloat  cblas_snrm2 (  CInt N,   CFloat  *X,   CInt incX);
CDouble cblas_dnrm2 (  CInt N,   CDouble *X,   CInt incX);
CFloat  cblas_scnrm2(  CInt N,   CFloat  *X,   CInt incX);
CDouble cblas_dznrm2(  CInt N,   CDouble *X,   CInt incX);

CBLAS_INDEX cblas_isamax(  CInt n,   CFloat  *x,   CInt incx);
CBLAS_INDEX cblas_idamax(  CInt n,   CDouble *x,   CInt incx);
CBLAS_INDEX cblas_icamax(  CInt n,   CFloat  *x,   CInt incx);
CBLAS_INDEX cblas_izamax(  CInt n,   CDouble *x,   CInt incx);

void cblas_saxpy(  CInt n,   CFloat alpha,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_daxpy(  CInt n,   CDouble alpha,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);
void cblas_caxpy(  CInt n,   CFloat *alpha,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_zaxpy(  CInt n,   CDouble *alpha,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);

void cblas_scopy(  CInt n,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_dcopy(  CInt n,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);
void cblas_ccopy(  CInt n,   CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_zcopy(  CInt n,   CDouble *x,   CInt incx, CDouble *y,   CInt incy);

void cblas_sswap(  CInt n, CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_dswap(  CInt n, CDouble *x,   CInt incx, CDouble *y,   CInt incy);
void cblas_cswap(  CInt n, CFloat *x,   CInt incx, CFloat *y,   CInt incy);
void cblas_zswap(  CInt n, CDouble *x,   CInt incx, CDouble *y,   CInt incy);

void cblas_srot(  CInt N, CFloat *X,   CInt incX, CFloat *Y,   CInt incY,   CFloat c,   CFloat s);
void cblas_drot(  CInt N, CDouble *X,   CInt incX, CDouble *Y,   CInt incY,   CDouble c,   CDouble  s);

void cblas_srotg(CFloat *a, CFloat *b, CFloat *c, CFloat *s);
void cblas_drotg(CDouble *a, CDouble *b, CDouble *c, CDouble *s);

void cblas_srotm(  CInt N, CFloat *X,   CInt incX, CFloat *Y,   CInt incY,   CFloat *P);
void cblas_drotm(  CInt N, CDouble *X,   CInt incX, CDouble *Y,   CInt incY,   CDouble *P);

void cblas_srotmg(CFloat *d1, CFloat *d2, CFloat *b1,   CFloat b2, CFloat *P);
void cblas_drotmg(CDouble *d1, CDouble *d2, CDouble *b1,   CDouble b2, CDouble *P);

void cblas_sscal(  CInt N,   CFloat alpha, CFloat *X,   CInt incX);
void cblas_dscal(  CInt N,   CDouble alpha, CDouble *X,   CInt incX);
void cblas_cscal(  CInt N,   CFloat *alpha, CFloat *X,   CInt incX);
void cblas_zscal(  CInt N,   CDouble *alpha, CDouble *X,   CInt incX);
void cblas_csscal(  CInt N,   CFloat alpha, CFloat *X,   CInt incX);
void cblas_zdscal(  CInt N,   CDouble alpha, CDouble *X,   CInt incX);

void cblas_sgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
           CFloat alpha,   CFloat  *a,   CInt lda,    CFloat  *x,   CInt incx,    CFloat beta,  CFloat  *y,   CInt incy);
void cblas_dgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
           CDouble alpha,   CDouble  *a,   CInt lda,    CDouble  *x,   CInt incx,    CDouble beta,  CDouble  *y,   CInt incy);
void cblas_cgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
           CFloat *alpha,   CFloat  *a,   CInt lda,    CFloat  *x,   CInt incx,    CFloat *beta,  CFloat  *y,   CInt incy);
void cblas_zgemv(  enum CBLAS_ORDER order,    enum CBLAS_TRANSPOSE trans,    CInt m,   CInt n,
           CDouble *alpha,   CDouble  *a,   CInt lda,    CDouble  *x,   CInt incx,    CDouble *beta,  CDouble  *y,   CInt incy);

void cblas_sger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat   alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
void cblas_dger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble  alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
void cblas_cgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat  *alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
void cblas_cgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CFloat  *alpha,   CFloat  *X,   CInt incX,   CFloat  *Y,   CInt incY, CFloat  *A,   CInt lda);
void cblas_zgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
void cblas_zgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);

void cblas_strsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_dtrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
void cblas_ctrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_ztrsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);

void cblas_strmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_dtrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
void cblas_ctrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_ztrmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,   CInt N,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);

void cblas_ssyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A,   CInt lda);
void cblas_dsyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *A,   CInt lda);
void cblas_cher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A,   CInt lda);
void cblas_zher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *A,   CInt lda);

void cblas_ssyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,  CInt N,   CFloat alpha,   CFloat *X,
                  CInt incX,   CFloat *Y,   CInt incY, CFloat *A,   CInt lda);
void cblas_dsyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,
                  CInt incX,   CDouble *Y,   CInt incY, CDouble *A,   CInt lda);
void cblas_cher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *X,   CInt incX,
                  CFloat *Y,   CInt incY, CFloat *A,   CInt lda);
void cblas_zher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,
                  CDouble *Y,   CInt incY, CDouble *A,   CInt lda);

void cblas_sgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
                   CInt KL,   CInt KU,   CFloat alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
void cblas_dgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
                   CInt KL,   CInt KU,   CDouble alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);
void cblas_cgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
                   CInt KL,   CInt KU,   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
void cblas_zgbmv(  enum CBLAS_ORDER order,   enum CBLAS_TRANSPOSE TransA,   CInt M,   CInt N,
                   CInt KL,   CInt KU,   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);

void cblas_ssbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   CFloat alpha,   CFloat *A,
                   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
void cblas_dsbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   CDouble alpha,   CDouble *A,
                   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);


void cblas_stbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_dtbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
void cblas_ctbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_ztbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);

void cblas_stbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_dtbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);
void cblas_ctbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CFloat *A,   CInt lda, CFloat *X,   CInt incX);
void cblas_ztbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CInt K,   CDouble *A,   CInt lda, CDouble *X,   CInt incX);

void cblas_stpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
void cblas_dtpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);
void cblas_ctpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
void cblas_ztpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);

void cblas_stpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
void cblas_dtpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);
void cblas_ctpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CFloat *Ap, CFloat *X,   CInt incX);
void cblas_ztpsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
                   CInt N,   CDouble *Ap, CDouble *X,   CInt incX);

void cblas_ssymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *A,
                   CInt lda,   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
void cblas_dsymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *A,
                   CInt lda,   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);
void cblas_chemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *A,
                   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
void cblas_zhemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *A,
                   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);


void cblas_sspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *Ap,
                   CFloat *X,   CInt incX,   CFloat beta, CFloat *Y,   CInt incY);
void cblas_dspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *Ap,
                   CDouble *X,   CInt incX,   CDouble beta, CDouble *Y,   CInt incY);

void cblas_sspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *Ap);
void cblas_dspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX, CDouble *Ap);

void cblas_chpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX, CFloat *A);
void cblas_zhpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,  CInt incX, CDouble *A);

void cblas_sspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat alpha,   CFloat *X,   CInt incX,   CFloat *Y,   CInt incY, CFloat *A);
void cblas_dspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *A);
void cblas_chpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CFloat *alpha,   CFloat *X,   CInt incX,   CFloat *Y,   CInt incY, CFloat *Ap);
void cblas_zhpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CDouble *alpha,   CDouble *X,   CInt incX,   CDouble *Y,   CInt incY, CDouble *Ap);

void cblas_chbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
           CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
void cblas_zhbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
           CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);

void cblas_chpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
           CFloat *alpha,   CFloat *Ap,   CFloat *X,   CInt incX,   CFloat *beta, CFloat *Y,   CInt incY);
void cblas_zhpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
           CDouble *alpha,   CDouble *Ap,   CDouble *X,   CInt incX,   CDouble *beta, CDouble *Y,   CInt incY);

void cblas_sgemm(  enum CBLAS_ORDER Order,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_TRANSPOSE TransB,   CInt M,   CInt N,   CInt K,
           CFloat alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_dgemm(  enum CBLAS_ORDER Order,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_TRANSPOSE TransB,   CInt M,   CInt N,   CInt K,
           CDouble alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble beta, CDouble *C,   CInt ldc);
void cblas_cgemm(  enum CBLAS_ORDER Order,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_TRANSPOSE TransB,   CInt M,   CInt N,   CInt K,
           CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat *beta, CFloat *C,   CInt ldc);
void cblas_zgemm(  enum CBLAS_ORDER Order,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_TRANSPOSE TransB,   CInt M,   CInt N,   CInt K,
           CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble *beta, CDouble *C,   CInt ldc);

void cblas_ssymm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CFloat alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_dsymm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CDouble alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble beta, CDouble *C,   CInt ldc);
void cblas_csymm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat *beta, CFloat *C,   CInt ldc);
void cblas_zsymm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble *beta, CDouble *C,   CInt ldc);

void cblas_ssyrk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
           CInt N,   CInt K,   CFloat alpha,   CFloat *A,   CInt lda,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_dsyrk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
           CInt N,   CInt K,   CDouble alpha,   CDouble *A,   CInt lda,   CDouble beta, CDouble *C,   CInt ldc);
void cblas_csyrk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
           CInt N,   CInt K,   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *beta, CFloat *C,   CInt ldc);
void cblas_zsyrk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
           CInt N,   CInt K,   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *beta, CDouble *C,   CInt ldc);

void cblas_ssyr2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
            CInt N,   CInt K,   CFloat alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_dsyr2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
            CInt N,   CInt K,   CDouble alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble beta, CDouble *C,   CInt ldc);
void cblas_csyr2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
            CInt N,   CInt K,   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat *beta, CFloat *C,   CInt ldc);
void cblas_zsyr2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,
            CInt N,   CInt K,   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble *beta, CDouble *C,   CInt ldc);

void cblas_strmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
void cblas_dtrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);
void cblas_ctrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat *alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
void cblas_ztrmm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble *alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);

void cblas_strsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
void cblas_dtrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);
void cblas_ctrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CFloat *alpha,   CFloat *A,   CInt lda, CFloat *B,   CInt ldb);
void cblas_ztrsm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,
                   enum CBLAS_DIAG Diag,   CInt M,   CInt N,   CDouble *alpha,   CDouble *A,   CInt lda, CDouble *B,   CInt ldb);

void cblas_chemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat *beta, CFloat *C,   CInt ldc);
void cblas_zhemm(  enum CBLAS_ORDER Order,   enum CBLAS_SIDE Side,   enum CBLAS_UPLO Uplo,   CInt M,   CInt N,
                   CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble *beta, CDouble *C,   CInt ldc);

void cblas_cherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
                   CFloat alpha,   CFloat *A,   CInt lda,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_zherk(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
                   CDouble alpha,   CDouble *A,   CInt lda,   CDouble beta, CDouble *C,   CInt ldc);

void cblas_cher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
                    CFloat *alpha,   CFloat *A,   CInt lda,   CFloat *B,   CInt ldb,   CFloat beta, CFloat *C,   CInt ldc);
void cblas_zher2k(  enum CBLAS_ORDER Order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE Trans,   CInt N,   CInt K,
                    CDouble *alpha,   CDouble *A,   CInt lda,   CDouble *B,   CInt ldb,   CDouble beta, CDouble *C,   CInt ldc);

void cblas_xerbla(CInt p, char *rout, char *form, ...);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif
