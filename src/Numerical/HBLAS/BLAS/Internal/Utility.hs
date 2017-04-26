{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}
module Numerical.HBLAS.BLAS.Internal.Utility where

import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.MatrixTypes


-- | isVectorBadWithNIncrement returns true if the range of accesses is bigger
-- than the physical size of the underlying buffer
isVectorBadWithNIncrement ::Int {- logical size -} ->  Int {- buffer -}
      -> Int {- stride -} -> Bool
isVectorBadWithNIncrement logDim bufferDim  incx = bufferDim < (1 + (logDim-1) * incx)
{-
these should be converted to integer/natural internally so theres no overflow checks

-}


{-
import Data.Word (Word64)

--- this assumes all the ints are positive :)
isVectorBadWithNIncrement :: Word64 -> Word64 -> Word64 -> Bool
isVectorBadWithNIncrement bufferSize {- n -} logicalSize {- dim -} incx
    | bufferSize >=  0
      && logicalSize > 0  && incx >= 0
        = bufferSize <=  ((logicalSize - 1) * incx)


-}


vectorBadInfo :: String -> String -> Int -> Int -> Int -> String
vectorBadInfo funName matName dim n incx = "error info: Function " ++ funName ++ ": " ++ matName ++ " has buffer size of " ++ show dim ++ "\n stride "
     ++ show incx ++  "\n logical size "  ++ show n

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

