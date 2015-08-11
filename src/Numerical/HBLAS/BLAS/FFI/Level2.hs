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


-- perform the rank 1 operation   A := alpha*x*y' + A,

type GerxFunFFI scale el = CBLAS_ORDERT -> CInt -> CInt -> scale -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()

foreign import ccall unsafe "cblas_sger" cblas_sger_unsafe ::
        GerxFunFFI Float Float
foreign import ccall safe   "cblas_sger" cblas_sger_safe   ::
        GerxFunFFI Float Float

foreign import ccall unsafe "cblas_dger" cblas_dger_unsafe ::
        GerxFunFFI Double Double
foreign import ccall safe   "cblas_dger" cblas_dger_safe   ::
        GerxFunFFI Double Double

foreign import ccall unsafe "cblas_cgerc" cblas_cgerc_unsafe ::
        GerxFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall unsafe "cblas_zgerc" cblas_zgerc_unsafe ::
        GerxFunFFI (Ptr (Complex Double)) (Complex Double)

foreign import ccall safe   "cblas_cgerc" cblas_cgerc_safe   ::
        GerxFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe   "cblas_zgerc" cblas_zgerc_safe   ::
        GerxFunFFI (Ptr (Complex Double)) (Complex Double)

foreign import ccall unsafe "cblas_cgeru" cblas_cgeru_unsafe ::
        GerxFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall unsafe "cblas_zgeru" cblas_zgeru_unsafe ::
        GerxFunFFI (Ptr (Complex Double)) (Complex Double)

foreign import ccall safe   "cblas_cgeru" cblas_cgeru_safe   ::
        GerxFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe   "cblas_zgeru" cblas_zgeru_safe   ::
        GerxFunFFI (Ptr (Complex Double)) (Complex Double)

--void cblas_sger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   Float   alpha,   Float  *X,   CInt incX,   Float  *Y,   CInt incY, Float  *A,   CInt lda);
--void cblas_dger (  enum CBLAS_ORDER order,   CInt M,   CInt N,   Double  alpha,   Double *X,   CInt incX,   Double *Y,   CInt incY, Double *A,   CInt lda);
--void cblas_cgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   Float  *alpha,   Float  *X,   CInt incX,   Float  *Y,   CInt incY, Float  *A,   CInt lda);
--void cblas_cgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   Float  *alpha,   Float  *X,   CInt incX,   Float  *Y,   CInt incY, Float  *A,   CInt lda);
--void cblas_zgeru(  enum CBLAS_ORDER order,   CInt M,   CInt N,   Double *alpha,   Double *X,   CInt incX,   Double *Y,   CInt incY, Double *A,   CInt lda);
--void cblas_zgerc(  enum CBLAS_ORDER order,   CInt M,   CInt N,   Double *alpha,   Double *X,   CInt incX,   Double *Y,   CInt incY, Double *A,   CInt lda);

type HbmvFunFFI sc el =
       CBLAS_ORDERT -> CBLAS_UPLOT -> CInt -> CInt
    -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_chbmv"
    cblas_chbmv_unsafe :: HbmvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe   "cblas_chbmv"
    cblas_chbmv_safe   :: HbmvFunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zhbmv"
    cblas_zhbmv_unsafe :: HbmvFunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe   "cblas_zhbmv"
    cblas_zhbmv_safe   :: HbmvFunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_chbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
--           Float *alpha,   Float *A,   CInt lda,   Float *X,   CInt incX,   Float *beta, Float *Y,   CInt incY);
--void cblas_zhbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,
--           Double *alpha,   Double *A,   CInt lda,   Double *X,   CInt incX,   Double *beta, Double *Y,   CInt incY);

type HemvFunFFI sc el =
       CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
    -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_chemv"
    cblas_chemv_unsafe :: HemvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe   "cblas_chemv"
    cblas_chemv_safe   :: HemvFunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zhemv"
    cblas_zhemv_unsafe :: HemvFunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe   "cblas_zhemv"
    cblas_zhemv_safe   :: HemvFunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_chemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float *alpha,   Float *A,
--                   CInt lda,   Float *X,   CInt incX,   Float *beta, Float *Y,   CInt incY);
--void cblas_zhemv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double *alpha,   Double *A,
--                   CInt lda,   Double *X,   CInt incX,   Double *beta, Double *Y,   CInt incY);


type HerFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_cher"
    cblas_cher_unsafe :: HerFunFFI Float (Complex Float)
foreign import ccall safe "cblas_cher"
    cblas_cher_safe :: HerFunFFI Float (Complex Float)

foreign import ccall unsafe "cblas_zher"
    cblas_zher_unsafe :: HerFunFFI Double (Complex Double)
foreign import ccall safe "cblas_zher"
    cblas_zher_safe :: HerFunFFI Double (Complex Double)
--void cblas_cher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *X,   CInt incX, Float *A,   CInt lda);
--void cblas_zher(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,   CInt incX, Double *A,   CInt lda);

type Her2FunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_cher2"
    cblas_cher2_unsafe :: Her2FunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe "cblas_cher2"
    cblas_cher2_safe :: Her2FunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zher2"
    cblas_zher2_unsafe :: Her2FunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe "cblas_zher2"
    cblas_zher2_safe :: Her2FunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_cher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float *alpha,   Float *X,   CInt incX,
--                  Float *Y,   CInt incY, Float *A,   CInt lda);
--void cblas_zher2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double *alpha,   Double *X,   CInt incX,
--                  Double *Y,   CInt incY, Double *A,   CInt lda);

type HpmvFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_chpmv"
    cblas_chpmv_unsafe :: HpmvFunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe "cblas_chpmv"
    cblas_chpmv_safe :: HpmvFunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zhpmv"
    cblas_zhpmv_unsafe :: HpmvFunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe "cblas_zhpmv"
    cblas_zhpmv_safe :: HpmvFunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_chpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
--           Float *alpha,   Float *Ap,   Float *X,   CInt incX,   Float *beta, Float *Y,   CInt incY);
--void cblas_zhpmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,
--           Double *alpha,   Double *Ap,   Double *X,   CInt incX,   Double *beta, Double *Y,   CInt incY);

type HprFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> IO ()
foreign import ccall unsafe "cblas_chpr"
    cblas_chpr_unsafe :: HprFunFFI Float (Complex Float)
foreign import ccall safe "cblas_chpr"
    cblas_chpr_safe :: HprFunFFI Float (Complex Float)

foreign import ccall unsafe "cblas_zhpr"
    cblas_zhpr_unsafe :: HprFunFFI Double (Complex Double)
foreign import ccall safe "cblas_zhpr"
    cblas_zhpr_safe :: HprFunFFI Double (Complex Double)
--void cblas_chpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *X,   CInt incX, Float *A);
--void cblas_zhpr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,  CInt incX, Double *A);

type Hpr2FunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> IO ()
foreign import ccall unsafe "cblas_chpr2"
    cblas_chpr2_unsafe :: Hpr2FunFFI (Ptr (Complex Float)) (Complex Float)
foreign import ccall safe "cblas_chpr2"
    cblas_chpr2_safe :: Hpr2FunFFI (Ptr (Complex Float)) (Complex Float)

foreign import ccall unsafe "cblas_zhpr2"
    cblas_zhpr2_unsafe :: Hpr2FunFFI (Ptr (Complex Double)) (Complex Double)
foreign import ccall safe "cblas_zhpr2"
    cblas_zhpr2_safe :: Hpr2FunFFI (Ptr (Complex Double)) (Complex Double)
--void cblas_chpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float *alpha,   Float *X,   CInt incX,   Float *Y,   CInt incY, Float *Ap);
--void cblas_zhpr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double *alpha,   Double *X,   CInt incX,   Double *Y,   CInt incY, Double *Ap);

type SbmvFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_ssbmv"
    cblas_ssbmv_unsafe :: SbmvFunFFI Float Float
foreign import ccall safe "cblas_ssbmv"
    cblas_ssbmv_safe :: SbmvFunFFI Float Float

foreign import ccall unsafe "cblas_dsbmv"
    cblas_dsbmv_unsafe :: SbmvFunFFI Double Double
foreign import ccall safe "cblas_dsbmv"
    cblas_dsbmv_safe :: SbmvFunFFI Double Double
--void cblas_ssbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   Float alpha,   Float *A,
--                   CInt lda,   Float *X,   CInt incX,   Float beta, Float *Y,   CInt incY);
--void cblas_dsbmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   CInt K,   Double alpha,   Double *A,
--                   CInt lda,   Double *X,   CInt incX,   Double beta, Double *Y,   CInt incY);

---------------
--- | packed symmetric matrix * vector product  y:= alpha * Av  + beta * y
---------------
type SpmvFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> Ptr el -> CInt -> sc -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_sspmv"
    cblas_sspmv_unsafe :: SpmvFunFFI Float Float
foreign import ccall safe "cblas_sspmv"
    cblas_sspmv_safe :: SpmvFunFFI Float Float

foreign import ccall unsafe "cblas_dspmv"
    cblas_dspmv_unsafe :: SpmvFunFFI Double Double
foreign import ccall safe "cblas_dspmv"
    cblas_dspmv_safe :: SpmvFunFFI Double Double
--void cblas_sspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *Ap,
--                   Float *X,   CInt incX,   Float beta, Float *Y,   CInt incY);
--void cblas_dspmv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *Ap,
--                   Double *X,   CInt incX,   Double beta, Double *Y,   CInt incY);

type SprFunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> IO ()
foreign import ccall unsafe "cblas_sspr"
    cblas_sspr_unsafe :: SprFunFFI Float Float
foreign import ccall safe "cblas_sspr"
    cblas_sspr_safe :: SprFunFFI Float Float

foreign import ccall unsafe "cblas_dspr"
    cblas_dspr_unsafe :: SprFunFFI Double Double
foreign import ccall safe "cblas_dspr"
    cblas_dspr_safe :: SprFunFFI Double Double
--void cblas_sspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *X,   CInt incX, Float *Ap);
--void cblas_dspr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,   CInt incX, Double *Ap);


type Spr2FunFFI sc el =
      CBLAS_ORDERT -> CBLAS_UPLOT -> CInt
      -> sc -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> IO ()
foreign import ccall unsafe "cblas_sspr2"
    cblas_sspr2_unsafe :: Spr2FunFFI Float Float
foreign import ccall safe "cblas_sspr2"
    cblas_sspr2_safe :: Spr2FunFFI Float Float

foreign import ccall unsafe "cblas_dspr2"
    cblas_dspr2_unsafe :: Spr2FunFFI Double Double
foreign import ccall safe "cblas_dspr2"
    cblas_dspr2_safe :: Spr2FunFFI Double Double
--void cblas_sspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *X,   CInt incX,   Float *Y,   CInt incY, Float *A);
--void cblas_dspr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,   CInt incX,   Double *Y,   CInt incY, Double *A);


----------------------------------
---- |  (unpacked) symmetric matrix vector product    x:=Av, writes result x into v
---------------------------------

type SymvFunFFI el = CBLAS_ORDERT -> CBLAS_UPLOT -> CInt -> el -> Ptr el ->  CInt ->
                        Ptr el -> CInt -> el -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_ssymv"
    cblas_ssymv_unsafe :: SymvFunFFI Float
foreign import ccall safe "cblas_ssymv"
    cblas_ssymv_safe :: SymvFunFFI Float

foreign import ccall unsafe "cblas_dsymv"
    cblas_dsymv_unsafe :: SymvFunFFI Double
foreign import ccall safe "cblas_dsymv"
    cblas_dsymv_safe :: SymvFunFFI Double
--void cblas_ssymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *A,
--                   CInt lda,   Float *X,   CInt incX,   Float beta, Float *Y,   CInt incY);
--void cblas_dsymv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *A,
--                   CInt lda,   Double *X,   CInt incX,   Double beta, Double *Y,   CInt incY);

type SyrFunFFI el = CBLAS_ORDERT -> CBLAS_UPLOT -> CInt -> el -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_ssyr"
      cblas_ssyr_unsafe :: SyrFunFFI Float
foreign import ccall safe "cblas_ssyr"
      cblas_ssyr_safe :: SyrFunFFI Float

foreign import ccall unsafe "cblas_dsyr"
      cblas_dsyr_unsafe :: SyrFunFFI Double
foreign import ccall safe "cblas_dsyr"
      cblas_dsyr_safe :: SyrFunFFI Double
--void cblas_ssyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Float alpha,   Float *X,   CInt incX, Float *A,   CInt lda);
--void cblas_dsyr(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,   CInt incX, Double *A,   CInt lda);

type Syr2FunFFI el = CBLAS_ORDERT -> CBLAS_UPLOT -> CInt -> el -> Ptr el -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_ssyr2"
      cblas_ssyr2_unsafe :: Syr2FunFFI Float
foreign import ccall safe "cblas_ssyr2"
      cblas_ssyr2_safe :: Syr2FunFFI Float

foreign import ccall unsafe "cblas_dsyr2"
      cblas_dsyr2_unsafe :: Syr2FunFFI Double
foreign import ccall safe "cblas_dsyr2"
      cblas_dsyr2_safe :: Syr2FunFFI Double
--void cblas_ssyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,  CInt N,   Float alpha,   Float *X,
--                  CInt incX,   Float *Y,   CInt incY, Float *A,   CInt lda);
--void cblas_dsyr2(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   CInt N,   Double alpha,   Double *X,
--                  CInt incX,   Double *Y,   CInt incY, Double *A,   CInt lda);

----------------
--- | solves  Ax=v where A is k+1 banded triangular matrix, and x and
----------------
type TbsvFunFFI el = CBLAS_ORDERT -> CBLAS_UPLOT -> CBLAS_TRANSPOSET -> CBLAS_DIAGT ->
                      CInt -> CInt -> Ptr el -> CInt -> Ptr el -> CInt -> IO ()
foreign import ccall unsafe "cblas_stbsv"
      cblas_stbsv_unsafe :: TbsvFunFFI Float
foreign import ccall safe "cblas_stbsv"
      cblas_stbsv_safe :: TbsvFunFFI Float

foreign import ccall unsafe "cblas_dtbsv"
      cblas_dtbsv_unsafe :: TbsvFunFFI Double
foreign import ccall safe "cblas_dtbsv"
      cblas_dtbsv_safe :: TbsvFunFFI Double

foreign import ccall unsafe "cblas_ctbsv"
      cblas_ctbsv_unsafe :: TbsvFunFFI (Complex Float)
foreign import ccall safe "cblas_ctbsv"
      cblas_ctbsv_safe :: TbsvFunFFI (Complex Float)

foreign import ccall unsafe "cblas_ztbsv"
      cblas_ztbsv_unsafe :: TbsvFunFFI (Complex Double)
foreign import ccall safe "cblas_ztbsv"
      cblas_ztbsv_safe :: TbsvFunFFI (Complex Double)
--void cblas_stbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   Float *A,   CInt lda, Float *X,   CInt incX);
--void cblas_dtbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   Double *A,   CInt lda, Double *X,   CInt incX);
--void cblas_ctbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   Float *A,   CInt lda, Float *X,   CInt incX);
--void cblas_ztbsv(  enum CBLAS_ORDER order,   enum CBLAS_UPLO Uplo,   enum CBLAS_TRANSPOSE TransA,   enum CBLAS_DIAG Diag,
--                   CInt N,   CInt K,   Double *A,   CInt lda, Double *X,   CInt incX);

