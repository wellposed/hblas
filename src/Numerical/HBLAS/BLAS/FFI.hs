{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}


module Numerical.HBLAS.BLAS.FFI  where

import Foreign()
import Foreign.C.Types

-- /*Set the number of threads on runtime.*/
--foreign import ccall unsafe "openblas_set_num_threads" openblas_set_num_threads_unsafe :: CInt -> IO ()

--foreign import ccall unsafe "goto_set_num_threads" goto_set_num_threads_unsafe :: CInt -> IO ()

{- | For All of the BlAS FFI operations,


-}
{-


typedef enum CBLAS_ORDER     {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
typedef enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, CblasConjNoTrans=114} CBLAS_TRANSPOSE;
typedef enum CBLAS_UPLO      {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
typedef enum CBLAS_DIAG      {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
typedef enum CBLAS_SIDE      {CblasLeft=141, CblasRight=142} CBLAS_SIDE;

-}

newtype CBLAS_INDEX = CBIndex CSize
        deriving (Eq,Show)
newtype CBLAS_ORDERT = CBOInt CInt
    deriving (Eq,Show)
data BLASOrder = BLASRowMajor | BLASColMajor
    deriving (Eq,Show)

encodeOrder :: BLASOrder -> CBLAS_ORDERT
encodeOrder BLASRowMajor = CBOInt 101
encodeOrder BLASColMajor = CBOInt 102

newtype CBLAS_TRANSPOSET = CBLAS_TransposeT{ unCBLAS_TransposeT :: CInt } deriving (Eq, Show)

data BLAS_Transpose = BlasNoTranspose | BlasTranspose | BlasConjTranspose | BlasConjNoTranspose

encodeTranspose :: BLAS_Transpose -> CBLAS_TRANSPOSET
encodeTranspose  BlasNoTranspose = CBLAS_TransposeT 111
encodeTranspose  BlasTranspose = CBLAS_TransposeT 112
encodeTranspose  BlasConjTranspose =  CBLAS_TransposeT 113
encodeTranspose  BlasConjNoTranspose = CBLAS_TransposeT 114

newtype CBLAS_UPLOT = CBlasUPLO CInt
    deriving (Eq,Show)
data BLASUplo = BUpper | BLower
    deriving (Eq,Show)

encodeUPLO :: BLASUplo -> CBLAS_UPLOT
encodeUPLO BUpper = CBlasUPLO 121
encodeUPLO BLower = CBlasUPLO 122

newtype CBLAS_DIAGT = CBLAS_DiagT CUChar
    deriving (Show,Eq)


data BlasDiag = BlasNonUnit   | BlasUnit
    deriving (Eq,Show )

encodeDiag :: BlasDiag -> CBLAS_DIAGT
encodeDiag BlasNonUnit = CBLAS_DiagT 131
encodeDiag BlasUnit = CBLAS_DiagT 132

newtype CBLAS_SIDET = CBLAS_SideT { unCBLAS_SideT :: CUChar }
    deriving (Eq, Show)
data BlasSide = BlasLeft | BlasRight
    deriving (Eq,Show)
encodeSide :: BlasSide -> CBLAS_SIDET
encodeSide BlasLeft = CBLAS_SideT 141
encodeSide BlasRight = CBLAS_SideT 142


--typedef enum CBLAS_ORDER     {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
--typedef enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, CblasConjNoTrans=114} CBLAS_TRANSPOSE;
--typedef enum CBLAS_UPLO      {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
--typedef enum CBLAS_DIAG      {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
--typedef enum CBLAS_SIDE      {CblasLeft=141, CblasRight=142} CBLAS_SIDE;




