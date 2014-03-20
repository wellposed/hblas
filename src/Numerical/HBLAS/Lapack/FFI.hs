

module Numerical.HBLAS.Lapack.FFI where




{-

stylenote: we will not use the LAPACKE_* operations, only the
LAPACKE_*_work variants that require an explicitly provided work buffer.

This is to ensure that solver routine allocation behavior is transparent 


-}



{-
void LAPACK_dgesvx( char* fact, char* trans, lapack_int* n, lapack_int* nrhs,
                    double* a, lapack_int* lda, double* af, lapack_int* ldaf,
                    lapack_int* ipiv, char* equed, double* r, double* c,
                    double* b, lapack_int* ldb, double* x, lapack_int* ldx,
                    double* rcond, double* ferr, double* berr, double* work,
                    lapack_int* iwork, lapack_int *info );



-}


{-
    fortran FFI conventions!
-}
type Fun_FFI_GESVX el = Ptr Fact_C -> Ptr Trans_C -> Ptr Size_C -> Ptr Stride_C -> 
    Ptr el -> Ptr Stride_C -> Ptr Double -> Ptr Stride_C ->
    Ptr Int32 -> Ptr Equilib_C {- equed -} -> Ptr el {- r -} -> Ptr el  ->
    Ptr el {- b -} -> Ptr Stride_C {- ld b   -} -> Ptr el {- x -} -> Ptr Stride_C {- ldx -}-> 

{-

the prefixes mean s=single,d=double,c=complex float,d=complex double



fact will be a 1 character C string 
either 
    "F", then the inputs af and ipiv already contain the permuted LU factorization 
        (act as input rather than result params)
    "E", Matrix input A will be equilibriated if needed, then copied to AF and Factored
    "N", matrix input A will be copied to AF 

-}


{-
Xgesvx  is the s -sing

-}

foreign import ccall  LAPACKE_sgesvx_work ,CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat  -> CInt -> Ptr CFloat  -> CInt -> Ptr CInt -> CString -> Ptr CFloat  -> Ptr CFloat  -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt   -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
foreign import ccall LAPACKE_dgesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
foreign import ccall "LAPACKE_cgesvx_work" CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat  -> CInt -> Ptr CFloat  -> CInt -> Ptr CInt -> CString -> Ptr CFloat  -> Ptr CFloat  -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt   -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "LAPACKE_zgesvx_work" CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

