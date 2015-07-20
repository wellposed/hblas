{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}
module Numerical.HBLAS.BLAS.Internal.Utility where

import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.MatrixTypes

isVectorBadWithNIncrement :: Int -> Int -> Int -> Bool
isVectorBadWithNIncrement dim n incx = dim < (1 + (n-1) * incx)

vectorBadInfo :: String -> String -> Int -> Int -> Int -> String
vectorBadInfo funName matName dim n incx = "Function " ++ funName ++ ": " ++ matName ++ " constains too few elements of " ++ show dim ++ " and " ++ show (1 + (n-1) * incx) ++ " elements are needed."

coordSwapper :: Transpose -> (a,a)-> (a,a)
coordSwapper NoTranspose (a,b) = (a,b)
coordSwapper ConjNoTranspose (a,b) = (a,b)
coordSwapper Transpose (a,b) = (b,a)
coordSwapper ConjTranspose (a,b) = (b,a)

encodeNiceOrder :: SOrientation x  -> CBLAS_ORDERT
encodeNiceOrder SRow= encodeOrder  BLASRowMajor
encodeNiceOrder SColumn= encodeOrder BLASColMajor


encodeFFITranspose :: Transpose -> CBLAS_TRANSPOSET
encodeFFITranspose  x=  encodeTranspose $ encodeNiceTranspose x

encodeNiceTranspose :: Transpose -> BLAS_Transpose
encodeNiceTranspose x = case x of
        NoTranspose -> BlasNoTranspose
        Transpose -> BlasTranspose
        ConjTranspose -> BlasConjTranspose
        ConjNoTranspose -> BlasConjNoTranspose

encodeFFIMatrixHalf :: MatUpLo -> CBLAS_UPLOT
encodeFFIMatrixHalf x = encodeUPLO $ encodeNiceUPLO x

encodeNiceUPLO :: MatUpLo -> BLASUplo
encodeNiceUPLO x = case x of
                    MatUpper  -> BUpper
                    MatLower  -> BLower

encodeFFITriangleSort :: MatDiag -> CBLAS_DIAGT
encodeFFITriangleSort x = encodeDiag $ encodeNiceDIAG x

encodeNiceDIAG :: MatDiag -> BlasDiag
encodeNiceDIAG x = case x of
                    MatUnit    -> BlasUnit
                    MatNonUnit -> BlasNonUnit

encodeFFISide :: EquationSide -> CBLAS_SIDET
encodeFFISide x = encodeSide $ encodeNiceSide x

encodeNiceSide :: EquationSide -> BlasSide
encodeNiceSide x = case x of
                    LeftSide -> BlasLeft
                    RightSide -> BlasRight

