{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "lapacke.h"
module LAPACKE.Lapacke where
import Foreign.Ptr
#strict_import

import LAPACKE.LapackeConfig
import LAPACKE.LapackeMangling
#ccall lapack_make_complex_float , CFloat -> CFloat -> IO CFloat
#ccall lapack_make_complex_double , CDouble -> CDouble -> IO CDouble
#callback LAPACK_S_SELECT2 , Ptr CFloat -> Ptr CFloat -> IO CInt
#callback LAPACK_S_SELECT3 , Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#callback LAPACK_D_SELECT2 , Ptr CDouble -> Ptr CDouble -> IO CInt
#callback LAPACK_D_SELECT3 , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#callback LAPACK_C_SELECT1 , Ptr CFloat -> IO CInt
#callback LAPACK_C_SELECT2 , Ptr CFloat -> Ptr CFloat -> IO CInt
#callback LAPACK_Z_SELECT1 , Ptr CDouble -> IO CInt
#callback LAPACK_Z_SELECT2 , Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall lsame_ , CString -> CString -> CInt -> CInt -> IO CInt
#ccall LAPACKE_sbdsdc , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dbdsdc , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sbdsqr , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dbdsqr , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cbdsqr , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zbdsqr , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sdisna , CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ddisna , CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbbrd , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgbbrd , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgbbrd , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgbbrd , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgbcon , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbcon , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbcon , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbcon , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbequ , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbequ , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbequ , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbequ , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbequb , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbequb , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbequb , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbequb , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbsv , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgbsv , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgbsv , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgbsv , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbsvxx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbsvxx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbsvxx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbsvxx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbtrf , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbtrf , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbtrf , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgbtrf , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgebak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgebak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgebak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgebak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgebal , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgebal , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgebal , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgebal , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgebrd , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgebrd , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgebrd , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgebrd , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgecon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgecon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgecon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgecon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeequ , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeequ , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeequ , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeequ , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeequb , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeequb , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeequb , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeequb , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgees , CInt -> CChar -> CChar -> <LAPACK_S_SELECT2> -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgees , CInt -> CChar -> CChar -> <LAPACK_D_SELECT2> -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgees , CInt -> CChar -> CChar -> <LAPACK_C_SELECT1> -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgees , CInt -> CChar -> CChar -> <LAPACK_Z_SELECT1> -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeesx , CInt -> CChar -> CChar -> <LAPACK_S_SELECT2> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeesx , CInt -> CChar -> CChar -> <LAPACK_D_SELECT2> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeesx , CInt -> CChar -> CChar -> <LAPACK_C_SELECT1> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeesx , CInt -> CChar -> CChar -> <LAPACK_Z_SELECT1> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgehrd , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgehrd , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgehrd , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgehrd , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgejsv , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgejsv , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgelq2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgelq2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgelq2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgelq2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgelqf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgelqf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgelqf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgelqf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgels , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgels , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgels , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgels , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgelsd , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgelsd , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgelsd , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgelsd , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgelss , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgelss , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgelss , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgelss , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgelsy , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgelsy , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgelsy , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgelsy , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgeqlf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqlf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqlf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqlf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqp3 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqp3 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqp3 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqp3 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqpf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqpf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqpf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqpf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqr2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqr2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqr2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqr2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqrf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqrf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqrf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqrf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqrfp , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqrfp , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqrfp , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqrfp , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgerfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgerfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgerfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgerfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgerfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgerfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgerfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgerfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgerqf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgerqf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgerqf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgerqf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesdd , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgesdd , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgesdd , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgesdd , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgesv , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgesv , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgesv , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgesv , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsgesv , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zcgesv , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgesvd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgesvd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgesvd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesvj , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgesvj , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgetf2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgetf2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgetf2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgetf2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgetrf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgetrf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgetrf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgetrf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgetri , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgetri , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgetri , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgetri , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgetrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgetrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgetrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgetrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggbak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggbak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggbak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggbak , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggbal , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggbal , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggbal , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggbal , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgges , CInt -> CChar -> CChar -> CChar -> <LAPACK_S_SELECT3> -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgges , CInt -> CChar -> CChar -> CChar -> <LAPACK_D_SELECT3> -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgges , CInt -> CChar -> CChar -> CChar -> <LAPACK_C_SELECT2> -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgges , CInt -> CChar -> CChar -> CChar -> <LAPACK_Z_SELECT2> -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggesx , CInt -> CChar -> CChar -> CChar -> <LAPACK_S_SELECT3> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggesx , CInt -> CChar -> CChar -> CChar -> <LAPACK_D_SELECT3> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggesx , CInt -> CChar -> CChar -> CChar -> <LAPACK_C_SELECT2> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggesx , CInt -> CChar -> CChar -> CChar -> <LAPACK_Z_SELECT2> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggevx , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggglm , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggglm , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggglm , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggglm , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgghrd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgghrd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgghrd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgghrd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgglse , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgglse , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgglse , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgglse , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggqrf , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggqrf , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggqrf , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggqrf , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggrqf , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggrqf , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggrqf , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggrqf , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggsvd , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dggsvd , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cggsvd , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zggsvd , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sggsvp , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggsvp , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggsvp , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggsvp , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgtcon , CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgtcon , CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgtcon , CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtcon , CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgtrfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgtrfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgtrfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtrfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgtsv , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgtsv , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgtsv , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgtsv , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgtsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgtsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgtsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgttrf , CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgttrf , CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgttrf , CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgttrf , CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgttrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgttrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgttrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgttrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbev , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbev , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbevd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbevd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbevx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhbevx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chbgst , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbgst , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbgv , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbgv , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbgvd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbgvd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbgvx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhbgvx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chbtrd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhbtrd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_checon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhecon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheequb , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zheequb , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zheev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheevd , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zheevd , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheevr , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zheevr , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cheevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zheevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chegst , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhegst , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chegv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhegv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chegvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhegvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chegvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhegvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cherfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zherfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cherfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zherfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chesv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhesv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhesvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhesvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chetrd , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhetrd , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chetrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhetrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chetri , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhetri , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chetrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhetrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chfrk , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhfrk , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_shgeqz , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dhgeqz , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chgeqz , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhgeqz , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpcon , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpcon , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpevd , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpevd , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhpevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chpgst , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpgst , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpgv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpgv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpgvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpgvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpgvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhpgvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chprfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhprfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpsv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpsv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chptrd , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhptrd , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chptrf , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhptrf , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_chptri , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhptri , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_chptrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhptrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_shsein , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dhsein , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chsein , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhsein , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_shseqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dhseqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chseqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhseqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clacgv , CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlacgv , CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slacpy , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlacpy , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clacpy , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlacpy , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zlag2c , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_slag2d , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dlag2s , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_clag2z , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slagge , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dlagge , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_clagge , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zlagge , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_slamch , CChar -> IO CFloat
#ccall LAPACKE_dlamch , CChar -> IO CDouble
#ccall LAPACKE_slange , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_dlange , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_clange , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_zlange , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_clanhe , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_zlanhe , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_slansy , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_dlansy , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_clansy , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_zlansy , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_slantr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_dlantr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_clantr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CFloat
#ccall LAPACKE_zlantr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CDouble
#ccall LAPACKE_slarfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlarfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clarfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlarfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slarfg , CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarfg , CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarfg , CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarfg , CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slarft , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlarft , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clarft , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlarft , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slarfx , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarfx , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarfx , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarfx , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slarnv , CInt -> Ptr CInt -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarnv , CInt -> Ptr CInt -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarnv , CInt -> Ptr CInt -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarnv , CInt -> Ptr CInt -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slaset , CInt -> CChar -> CInt -> CInt -> CFloat -> CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlaset , CInt -> CChar -> CInt -> CInt -> CDouble -> CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_claset , CInt -> CChar -> CInt -> CInt -> CFloat -> CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlaset , CInt -> CChar -> CInt -> CInt -> CDouble -> CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slasrt , CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlasrt , CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slaswp , CInt -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dlaswp , CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_claswp , CInt -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zlaswp , CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_slatms , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CChar -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlatms , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CChar -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clatms , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CChar -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlatms , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CChar -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slauum , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlauum , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clauum , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlauum , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sopgtr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dopgtr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sopmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dopmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorgbr , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorgbr , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorghr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorghr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorglq , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorglq , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorgql , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorgql , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorgqr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorgqr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorgrq , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorgrq , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorgtr , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dorgtr , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sormbr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormbr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormhr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormhr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormlq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormlq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormql , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormql , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormrq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormrq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormrz , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormrz , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbcon , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpbcon , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpbcon , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbcon , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbequ , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpbequ , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpbequ , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbequ , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbrfs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbstf , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbstf , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbstf , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbstf , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbsv , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbsv , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbsv , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbsv , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbsvx , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbtrf , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbtrf , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbtrf , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbtrf , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbtrs , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spftrf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpftrf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpftrf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpftrf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spftri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpftri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpftri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpftri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spftrs , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpftrs , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpftrs , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpftrs , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spocon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpocon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpocon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpocon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spoequ , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpoequ , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpoequ , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpoequ , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spoequb , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpoequb , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpoequb , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpoequb , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sporfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dporfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cporfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zporfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sporfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dporfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cporfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zporfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sposv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dposv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cposv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zposv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsposv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zcposv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sposvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dposvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cposvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zposvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sposvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dposvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cposvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zposvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spotrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spotri , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotri , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotri , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotri , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spotrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sppcon , CInt -> CChar -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dppcon , CInt -> CChar -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cppcon , CInt -> CChar -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppcon , CInt -> CChar -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sppequ , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dppequ , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cppequ , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppequ , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spprfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpprfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpprfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpprfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sppsv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dppsv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cppsv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zppsv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sppsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dppsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cppsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptrf , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpptrf , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpptrf , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpptrf , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptri , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpptri , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpptri , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpptri , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpptrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpptrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpptrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spstrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> CFloat -> IO CInt
#ccall LAPACKE_dpstrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> CDouble -> IO CInt
#ccall LAPACKE_cpstrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> CFloat -> IO CInt
#ccall LAPACKE_zpstrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> CDouble -> IO CInt
#ccall LAPACKE_sptcon , CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptcon , CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptcon , CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptcon , CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spteqr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpteqr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpteqr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpteqr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sptrfs , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptrfs , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptrfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptrfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sptsv , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dptsv , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cptsv , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zptsv , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sptsvx , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptsvx , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptsvx , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptsvx , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spttrf , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpttrf , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpttrf , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpttrf , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spttrs , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpttrs , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpttrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpttrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbev , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbev , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbevd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbevd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbevx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsbevx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssbgst , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbgst , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbgv , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbgv , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbgvd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbgvd , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbgvx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsbgvx , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssbtrd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsbtrd , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssfrk , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsfrk , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspcon , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspcon , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cspcon , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zspcon , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspevd , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspevd , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sspgst , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspgst , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspgv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspgv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspgvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspgvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspgvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspgvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssprfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsprfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csprfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsprfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspsv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspsv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cspsv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zspsv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cspsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zspsvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssptrd , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsptrd , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssptrf , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsptrf , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csptrf , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsptrf , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssptri , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsptri , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csptri , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsptri , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssptrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsptrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csptrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsptrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sstebz , CChar -> CChar -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstebz , CChar -> CChar -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstedc , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dstedc , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cstedc , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zstedc , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sstegr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstegr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cstegr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zstegr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstein , CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstein , CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cstein , CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zstein , CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstemr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstemr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cstemr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zstemr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssteqr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsteqr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csteqr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsteqr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssterf , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsterf , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sstev , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dstev , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sstevd , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dstevd , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sstevr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstevr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstevx , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstevx , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssycon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsycon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csycon , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsycon , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyequb , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyequb , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csyequb , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyequb , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyev , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyev , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyevd , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyevd , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyevr , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyevr , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssyevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyevx , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssygst , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsygst , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssygv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsygv , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssygvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsygvd , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssygvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsygvx , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssyrfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyrfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csyrfs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyrfs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csyrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyrfsx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssysv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsysv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csysv , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsysv , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssysvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsysvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csysvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsysvx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssysvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsysvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csysvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsysvxx , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssytrd , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsytrd , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssytrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsytrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_csytrf , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsytrf , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssytri , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsytri , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_csytri , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsytri , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssytrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsytrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csytrs , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsytrs , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stbcon , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtbcon , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctbcon , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztbcon , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stbrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtbrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctbrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztbrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stbtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtbtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctbtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztbtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stfsm , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtfsm , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctfsm , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztfsm , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stftri , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtftri , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctftri , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztftri , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stfttp , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtfttp , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctfttp , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztfttp , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stfttr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtfttr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctfttr , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztfttr , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stgevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_stgexc , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgexc , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgexc , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ztgexc , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_stgsen , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtgsen , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctgsen , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztgsen , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stgsja , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgsja , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgsja , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgsja , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_stgsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_stgsyl , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtgsyl , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctgsyl , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztgsyl , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stpcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtpcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctpcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stprfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtprfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctprfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztprfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stptri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtptri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctptri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztptri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stptrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtptrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctptrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztptrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stpttf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtpttf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctpttf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpttf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stpttr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtpttr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpttr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpttr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrcon , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztrevc , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_strexc , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrexc , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrexc , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ztrexc , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_strrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrrfs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strsen , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrsen , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrsen , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrsen , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztrsna , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_strsyl , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrsyl , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrsyl , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrsyl , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strtri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtrtri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctrtri , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztrtri , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtrtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctrtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztrtrs , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strttf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrttf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrttf , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrttf , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strttp , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrttp , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrttp , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrttp , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stzrzf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtzrzf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctzrzf , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztzrzf , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cungbr , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zungbr , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cunghr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zunghr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cunglq , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zunglq , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cungql , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zungql , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cungqr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zungqr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cungrq , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zungrq , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cungtr , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zungtr , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cunmbr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmbr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmhr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmhr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmlq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmlq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmql , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmql , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmqr , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmrq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmrq , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmrz , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmrz , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cupgtr , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zupgtr , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cupmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zupmtr , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sbdsdc_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dbdsdc_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sbdsqr_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dbdsqr_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cbdsqr_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zbdsqr_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sdisna_work , CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ddisna_work , CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbbrd_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbbrd_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbbrd_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbbrd_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbcon_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbcon_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbcon_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbcon_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbequ_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbequ_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbequ_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbequ_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbequb_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgbequb_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgbequb_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbequb_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbsv_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgbsv_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgbsv_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgbsv_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbsvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbsvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbsvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgbsvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgbtrf_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgbtrf_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgbtrf_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgbtrf_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgebak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgebak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgebak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgebak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgebal_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgebal_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgebal_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgebal_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgebrd_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgebrd_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgebrd_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgebrd_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgecon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgecon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgecon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgecon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeequ_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeequ_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeequ_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeequ_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeequb_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeequb_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeequb_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeequb_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgees_work , CInt -> CChar -> CChar -> <LAPACK_S_SELECT2> -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgees_work , CInt -> CChar -> CChar -> <LAPACK_D_SELECT2> -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgees_work , CInt -> CChar -> CChar -> <LAPACK_C_SELECT1> -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgees_work , CInt -> CChar -> CChar -> <LAPACK_Z_SELECT1> -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgeesx_work , CInt -> CChar -> CChar -> <LAPACK_S_SELECT2> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgeesx_work , CInt -> CChar -> CChar -> <LAPACK_D_SELECT2> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgeesx_work , CInt -> CChar -> CChar -> <LAPACK_C_SELECT1> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgeesx_work , CInt -> CChar -> CChar -> <LAPACK_Z_SELECT1> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgeev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgeevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgeevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgehrd_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgehrd_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgehrd_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgehrd_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgejsv_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgejsv_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgelq2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgelq2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgelq2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgelq2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgelqf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgelqf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgelqf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgelqf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgels_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgels_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgels_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgels_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgelsd_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgelsd_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgelsd_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgelsd_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgelss_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgelss_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgelss_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgelss_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgelsy_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgelsy_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgelsy_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgelsy_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqlf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqlf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqlf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqlf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqp3_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqp3_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqp3_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqp3_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqpf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqpf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqpf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqpf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqr2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqr2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqr2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqr2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqrf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqrfp_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrfp_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrfp_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrfp_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgerfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgerfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgerfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgerfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgerfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgerfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgerfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgerfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgerqf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgerqf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgerqf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgerqf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgesdd_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgesdd_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgesdd_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgesdd_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgesv_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgesv_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgesv_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgesv_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsgesv_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zcgesv_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgesvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgesvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgesvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesvj_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgesvj_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgetf2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgetf2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgetf2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgetf2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgetrf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgetrf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgetrf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgetrf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgetri_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgetri_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgetri_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgetri_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggbak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggbak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggbak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggbak_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggbal_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggbal_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggbal_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggbal_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgges_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_S_SELECT3> -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgges_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_D_SELECT3> -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgges_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_C_SELECT2> -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgges_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_Z_SELECT2> -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sggesx_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_S_SELECT3> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dggesx_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_D_SELECT3> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cggesx_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_C_SELECT2> -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zggesx_work , CInt -> CChar -> CChar -> CChar -> <LAPACK_Z_SELECT2> -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sggev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sggevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dggevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cggevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zggevx_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sggglm_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggglm_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggglm_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggglm_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgghrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgghrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgghrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgghrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgglse_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgglse_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgglse_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgglse_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggqrf_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggqrf_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggqrf_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggqrf_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggrqf_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dggrqf_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cggrqf_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zggrqf_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sggsvd_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dggsvd_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cggsvd_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zggsvd_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sggsvp_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dggsvp_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cggsvp_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zggsvp_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgtcon_work , CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgtcon_work , CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgtcon_work , CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtcon_work , CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgtrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgtrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgtrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgtsv_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgtsv_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgtsv_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgtsv_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgtsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgtsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgtsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgtsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgttrf_work , CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dgttrf_work , CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cgttrf_work , CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zgttrf_work , CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chbev_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhbev_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chbevd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhbevd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chbevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhbevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chbgst_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhbgst_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chbgv_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhbgv_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chbgvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhbgvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chbgvx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhbgvx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chbtrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhbtrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_checon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhecon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheequb_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zheequb_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zheev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cheevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zheevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_cheevr_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zheevr_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_cheevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zheevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chegst_work , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhegst_work , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chegv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhegv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chegvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhegvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chegvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhegvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cherfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zherfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cherfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zherfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chesv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhesv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhesvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhesvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chetrd_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhetrd_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chetrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhetrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chetri_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhetri_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhetrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chfrk_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhfrk_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_shgeqz_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dhgeqz_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chgeqz_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhgeqz_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpcon_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpcon_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhpevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chpevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhpevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chpgst_work , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpgst_work , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpgv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpgv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpgvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhpgvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chpgvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhpgvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chpsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhpsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chpsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhpsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chptrd_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhptrd_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chptrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhptrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_chptri_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zhptri_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_chptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_shsein_work , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dhsein_work , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chsein_work , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhsein_work , CInt -> CChar -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_shseqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dhseqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_chseqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zhseqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clacgv_work , CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlacgv_work , CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slacpy_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlacpy_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clacpy_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlacpy_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zlag2c_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_slag2d_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dlag2s_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_clag2z_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slagge_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlagge_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clagge_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlagge_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_claghe_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlaghe_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slagsy_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlagsy_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clagsy_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlagsy_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slapmr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dlapmr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_clapmr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zlapmr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_slartgp_work , CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlartgp_work , CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slartgs_work , CFloat -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlartgs_work , CDouble -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slapy2_work , CFloat -> CFloat -> IO CFloat
#ccall LAPACKE_dlapy2_work , CDouble -> CDouble -> IO CDouble
#ccall LAPACKE_slapy3_work , CFloat -> CFloat -> CFloat -> IO CFloat
#ccall LAPACKE_dlapy3_work , CDouble -> CDouble -> CDouble -> IO CDouble
#ccall LAPACKE_slamch_work , CChar -> IO CFloat
#ccall LAPACKE_dlamch_work , CChar -> IO CDouble
#ccall LAPACKE_slange_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_dlange_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_clange_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_zlange_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_clanhe_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_zlanhe_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_slansy_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_dlansy_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_clansy_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_zlansy_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_slantr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_dlantr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_clantr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CFloat
#ccall LAPACKE_zlantr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CDouble
#ccall LAPACKE_slarfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlarfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clarfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlarfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slarfg_work , CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarfg_work , CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarfg_work , CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarfg_work , CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slarft_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlarft_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clarft_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlarft_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slarfx_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarfx_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarfx_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarfx_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slarnv_work , CInt -> Ptr CInt -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlarnv_work , CInt -> Ptr CInt -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clarnv_work , CInt -> Ptr CInt -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlarnv_work , CInt -> Ptr CInt -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slaset_work , CInt -> CChar -> CInt -> CInt -> CFloat -> CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlaset_work , CInt -> CChar -> CInt -> CInt -> CDouble -> CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_claset_work , CInt -> CChar -> CInt -> CInt -> CFloat -> CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlaset_work , CInt -> CChar -> CInt -> CInt -> CDouble -> CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_slasrt_work , CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlasrt_work , CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slaswp_work , CInt -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dlaswp_work , CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_claswp_work , CInt -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zlaswp_work , CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_slatms_work , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CChar -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlatms_work , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CChar -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_clatms_work , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CChar -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zlatms_work , CInt -> CInt -> CInt -> CChar -> Ptr CInt -> CChar -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CChar -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slauum_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dlauum_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_clauum_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zlauum_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sopgtr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dopgtr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sopmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dopmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sorgbr_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorgbr_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorghr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorghr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorglq_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorglq_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorgql_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorgql_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorgqr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorgqr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorgrq_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorgrq_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sorgtr_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dorgtr_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormbr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormbr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormhr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormhr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormlq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormlq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormql_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormql_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormrq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormrq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormrz_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormrz_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sormtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dormtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbcon_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dpbcon_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cpbcon_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbcon_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbequ_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpbequ_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpbequ_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbequ_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dpbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cpbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbrfs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbstf_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbstf_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbstf_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbstf_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbsv_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbsv_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbsv_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbsv_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dpbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cpbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpbsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spbtrf_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbtrf_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbtrf_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbtrf_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpbtrs_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spftrf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpftrf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpftrf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpftrf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spftri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpftri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpftri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpftri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spftrs_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpftrs_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpftrs_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpftrs_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spocon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dpocon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cpocon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpocon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spoequ_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpoequ_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpoequ_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpoequ_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spoequb_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpoequb_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpoequb_work , CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpoequb_work , CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sporfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dporfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cporfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zporfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sporfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dporfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cporfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zporfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zcposv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sposvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dposvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cposvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zposvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sposvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dposvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cposvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zposvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spotrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spotri_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotri_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotri_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotri_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spotrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpotrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpotrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpotrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sppcon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dppcon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cppcon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppcon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sppequ_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dppequ_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cppequ_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppequ_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dpprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cpprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sppsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dppsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cppsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zppsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sppsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dppsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cppsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zppsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpptrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpptrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpptrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptri_work , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpptri_work , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpptri_work , CInt -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpptri_work , CInt -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_spstrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpstrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpstrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpstrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sptcon_work , CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptcon_work , CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptcon_work , CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptcon_work , CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spteqr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpteqr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpteqr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpteqr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sptrfs_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptrfs_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sptsv_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dptsv_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cptsv_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zptsv_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sptsvx_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dptsvx_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cptsvx_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zptsvx_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spttrf_work , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dpttrf_work , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cpttrf_work , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zpttrf_work , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_spttrs_work , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dpttrs_work , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cpttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zpttrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssbev_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsbev_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssbevd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsbevd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssbevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsbevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssbgst_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsbgst_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssbgv_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsbgv_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssbgvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsbgvd_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssbgvx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsbgvx_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssbtrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsbtrd_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssfrk_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> CInt -> CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsfrk_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> CInt -> CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspcon_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspcon_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cspcon_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zspcon_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dspevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sspevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sspgst_work , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspgst_work , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspgv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dspgv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspgvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dspgvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sspgvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspgvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsprfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sspsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dspsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cspsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zspsv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sspsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dspsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_cspsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zspsvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssptrd_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsptrd_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssptrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsptrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csptrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsptrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssptri_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsptri_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csptri_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsptri_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsptrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sstebz_work , CChar -> CChar -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstebz_work , CChar -> CChar -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstedc_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dstedc_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_cstedc_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zstedc_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sstegr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dstegr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_cstegr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zstegr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sstein_work , CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstein_work , CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_cstein_work , CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zstein_work , CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sstemr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dstemr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_cstemr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zstemr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssteqr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsteqr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csteqr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsteqr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssterf_work , CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsterf_work , CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sstev_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dstev_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sstevd_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dstevd_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sstevr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dstevr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_sstevx_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dstevx_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssycon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsycon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csycon_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsycon_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyequb_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsyequb_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csyequb_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyequb_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyev_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsyev_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssyevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsyevd_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssyevr_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsyevr_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssyevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyevx_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssygst_work , CInt -> CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsygst_work , CInt -> CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssygv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsygv_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssygvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsygvd_work , CInt -> CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssygvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> CInt -> CInt -> CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsygvx_work , CInt -> CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> CInt -> CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssyrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csyrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyrfs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssyrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csyrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsyrfsx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssysv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsysv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csysv_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsysv_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssysvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsysvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_csysvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsysvx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssysvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsysvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_csysvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsysvxx_work , CInt -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssytrd_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsytrd_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssytrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsytrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csytrf_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsytrf_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ssytri_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dsytri_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_csytri_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zsytri_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ssytrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dsytrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csytrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsytrs_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stbcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtbcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctbcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztbcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stbrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtbrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctbrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztbrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stbtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtbtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctbtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztbtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stfsm_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtfsm_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctfsm_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztfsm_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stftri_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtftri_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctftri_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztftri_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stfttp_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtfttp_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctfttp_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztfttp_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stfttr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtfttr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctfttr_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztfttr_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stgevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtgevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctgevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztgevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stgexc_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtgexc_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctgexc_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ztgexc_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_stgsen_work , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dtgsen_work , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ctgsen_work , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ztgsen_work , CInt -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_stgsja_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgsja_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgsja_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgsja_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_stgsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_stgsyl_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtgsyl_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctgsyl_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ztgsyl_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_stpcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtpcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctpcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stprfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtprfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctprfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztprfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stptri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtptri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctptri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztptri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stptrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtptrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctptrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztptrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stpttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtpttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctpttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stpttr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtpttr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpttr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpttr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrcon_work , CInt -> CChar -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrevc_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strexc_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrexc_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrexc_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ztrexc_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> IO CInt
#ccall LAPACKE_strrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrrfs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strsen_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dtrsen_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ctrsen_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztrsen_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dtrsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ctrsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrsna_work , CInt -> CChar -> CChar -> Ptr CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strsyl_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrsyl_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrsyl_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrsyl_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strtri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtrtri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctrtri_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztrtri_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtrtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctrtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztrtrs_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_strttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrttf_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_strttp_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtrttp_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctrttp_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztrttp_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stzrzf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtzrzf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctzrzf_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztzrzf_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cungbr_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zungbr_work , CInt -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunghr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunghr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunglq_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunglq_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cungql_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zungql_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cungqr_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zungqr_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cungrq_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zungrq_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cungtr_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zungtr_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmbr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmbr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmhr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmhr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmlq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmlq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmql_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmql_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmqr_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmrq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmrq_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmrz_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmrz_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cunmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zunmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cupgtr_work , CInt -> CChar -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zupgtr_work , CInt -> CChar -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cupmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zupmtr_work , CInt -> CChar -> CChar -> CChar -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_claghe , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zlaghe , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_slagsy , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dlagsy , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_clagsy , CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zlagsy , CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_slapmr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dlapmr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_clapmr , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zlapmr , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_slapy2 , CFloat -> CFloat -> IO CFloat
#ccall LAPACKE_dlapy2 , CDouble -> CDouble -> IO CDouble
#ccall LAPACKE_slapy3 , CFloat -> CFloat -> CFloat -> IO CFloat
#ccall LAPACKE_dlapy3 , CDouble -> CDouble -> CDouble -> IO CDouble
#ccall LAPACKE_slartgp , CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlartgp , CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_slartgs , CFloat -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dlartgs , CDouble -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cbbcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_cbbcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_cheswapr , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_cheswapr_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_chetri2 , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_chetri2_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_chetri2x , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_chetri2x_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_chetrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_chetrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_csyconv , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_csyconv_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_csyswapr , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_csyswapr_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_csytri2 , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_csytri2_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_csytri2x , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_csytri2x_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_csytrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_csytrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_cunbdb , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_cunbdb_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_cuncsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_cuncsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dbbcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_dbbcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dorbdb , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_dorbdb_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dorcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dorcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyconv , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsyconv_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_dsyswapr , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_dsyswapr_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_dsytri2 , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_dsytri2_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsytri2x , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_dsytri2x_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsytrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dsytrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sbbcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_sbbcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_sorbdb , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
#ccall LAPACKE_sorbdb_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_sorcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_sorcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssyconv , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssyconv_work , CInt -> CChar -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ssyswapr , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ssyswapr_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> CInt -> IO CInt
#ccall LAPACKE_ssytri2 , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_ssytri2_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ssytri2x , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_ssytri2x_work , CInt -> CChar -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ssytrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ssytrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zbbcsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_zbbcsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zheswapr , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_zheswapr_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_zhetri2 , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zhetri2_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zhetri2x , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zhetri2x_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zhetrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zhetrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_zsyconv , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsyconv_work , CInt -> CChar -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_zsyswapr , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_zsyswapr_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt
#ccall LAPACKE_zsytri2 , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_zsytri2_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zsytri2x , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> IO CInt
#ccall LAPACKE_zsytri2x_work , CInt -> CChar -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zsytrs2 , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zsytrs2_work , CInt -> CChar -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_zunbdb , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
#ccall LAPACKE_zunbdb_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zuncsd , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_zuncsd_work , CInt -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> IO CInt
#ccall LAPACKE_sgemqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgemqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgemqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgemqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqrt , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrt , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrt , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrt , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqrt2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrt2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrt2 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrt2 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqrt3 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrt3 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrt3 , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrt3 , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stpmqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtpmqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpmqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpmqrt , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_dtpqrt , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpqrt , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpqrt , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stpqrt2 , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtpqrt2 , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpqrt2 , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpqrt2 , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stprfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtprfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctprfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztprfb , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgemqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgemqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgemqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgemqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqrt_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dgeqrt_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_cgeqrt_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_zgeqrt_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_sgeqrt2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrt2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrt2_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrt2_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_sgeqrt3_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dgeqrt3_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_cgeqrt3_work , CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zgeqrt3_work , CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stpmqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_dtpmqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctpmqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpmqrt_work , CInt -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_dtpqrt_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_ctpqrt_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO CInt
#ccall LAPACKE_ztpqrt_work , CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> IO CInt
#ccall LAPACKE_stpqrt2_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtpqrt2_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctpqrt2_work , CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztpqrt2_work , CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_stprfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_dtprfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ctprfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_ztprfb_work , CInt -> CChar -> CChar -> CChar -> CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csyr , CInt -> CChar -> CInt -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsyr , CInt -> CChar -> CInt -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_csyr_work , CInt -> CChar -> CInt -> CFloat -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> IO CInt
#ccall LAPACKE_zsyr_work , CInt -> CChar -> CInt -> CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall LAPACKE_ilaver , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgetrf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgetrf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgetrf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgetrf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbtrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbtrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbtrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgbtrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgttrf_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgttrf_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgttrf_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgttrf_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spotrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpotrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpotrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpotrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpstrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spstrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpstrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpstrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpftrf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spftrf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpftrf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpftrf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall spptrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpptrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpptrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpptrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbtrf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbtrf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbtrf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpbtrf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spttrf_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpttrf_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpttrf_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpttrf_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssytrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csytrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsytrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssptrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsptrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csptrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsptrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chptrf_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhptrf_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spotrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpotrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpotrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpotrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpftrs_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spftrs_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpftrs_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpftrs_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpbtrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spttrs_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpttrs_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpttrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssytrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csytrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsytrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhptrs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall strtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztrtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stptrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtptrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctptrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztptrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stbtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtbtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctbtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztbtrs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgecon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgecon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgecon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgecon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgtcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgtcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgtcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgtcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spocon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpocon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpocon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpocon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sppcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dppcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cppcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zppcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpbcon_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sptcon_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dptcon_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cptcon_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zptcon_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssycon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsycon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csycon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsycon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall checon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhecon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sspcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cspcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zspcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chpcon_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhpcon_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stpcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtpcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctpcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztpcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stbcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtbcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctbcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztbcon_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgerfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgerfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgerfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgerfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgerfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgerfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgerfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgerfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgbrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgbrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgbrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgtrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgtrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgtrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgtrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sporfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dporfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cporfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zporfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dporfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sporfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zporfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cporfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall spprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpbrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sptrfs_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dptrfs_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cptrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zptrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssyrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csyrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsyrfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dsyrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssyrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsyrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csyrfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall cherfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zherfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall zherfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cherfsx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ssprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhprfs_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stprfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtprfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctprfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztprfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stbrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtbrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctbrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztbrfs_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgetri_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgetri_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgetri_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgetri_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spotri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpotri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpotri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpotri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpftri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spftri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpftri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpftri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall spptri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpptri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpptri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpptri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssytri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsytri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csytri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsytri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chetri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhetri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssptri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsptri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csptri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsptri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chptri_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhptri_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strtri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrtri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrtri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztrtri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtftri_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stftri_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztftri_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctftri_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall stptri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtptri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctptri_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztptri_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeequ_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgeequ_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgeequ_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeequ_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgeequb_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeequb_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeequb_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgeequb_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgbequ_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgbequ_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgbequ_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbequ_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgbequb_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgbequb_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbequb_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgbequb_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall spoequ_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpoequ_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpoequ_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpoequ_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dpoequb_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spoequb_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpoequb_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpoequb_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sppequ_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dppequ_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cppequ_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zppequ_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbequ_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpbequ_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpbequ_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpbequ_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dsyequb_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssyequb_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsyequb_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csyequb_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zheequb_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cheequb_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgesv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgesv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgesv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgesv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsgesv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zcgesv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgbsv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbsv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbsv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgbsv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dgbsvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbsvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgbsvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgbsvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgtsv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgtsv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgtsv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgtsv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgtsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgtsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgtsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgtsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zcposv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CFloat -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sposvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dposvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cposvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zposvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dposvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sposvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zposvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cposvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sppsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dppsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cppsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zppsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sppsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dppsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cppsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zppsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpbsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpbsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sptsv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dptsv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cptsv_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zptsv_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sptsvx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dptsvx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cptsvx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zptsvx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssysv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsysv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csysv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsysv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssysvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsysvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csysvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsysvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dsysvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssysvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsysvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csysvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall chesv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhesv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhesvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall zhesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chesvxx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sspsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cspsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zspsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cspsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zspsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chpsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhpsv_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chpsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhpsvx_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeqrf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqrf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqrf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeqrf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgeqpf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgeqpf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgeqpf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeqpf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeqp3_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqp3_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqp3_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeqp3_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sorgqr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorgqr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cungqr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zungqr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgelqf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgelqf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgelqf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgelqf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorglq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorglq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormlq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormlq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunglq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunglq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmlq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmlq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgeqlf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqlf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqlf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeqlf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorgql_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorgql_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cungql_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zungql_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormql_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormql_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmql_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmql_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgerqf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgerqf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgerqf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgerqf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorgrq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorgrq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cungrq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zungrq_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormrq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormrq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmrq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmrq_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stzrzf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtzrzf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctzrzf_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztzrzf_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormrz_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormrz_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmrz_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmrz_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggqrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggqrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggqrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggqrf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggrqf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggrqf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggrqf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggrqf_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgebrd_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgebrd_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgebrd_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgebrd_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgbbrd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgbbrd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgbbrd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgbbrd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sorgbr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorgbr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormbr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormbr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cungbr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zungbr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmbr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmbr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sbdsqr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dbdsqr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cbdsqr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zbdsqr_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sbdsdc_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dbdsdc_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssytrd_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytrd_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorgtr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorgtr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetrd_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetrd_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cungtr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zungtr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssptrd_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsptrd_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sopgtr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dopgtr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sopmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dopmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chptrd_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhptrd_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cupgtr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zupgtr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cupmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zupmtr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssbtrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsbtrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chbtrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhbtrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssterf_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsterf_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssteqr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsteqr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csteqr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsteqr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sstemr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstemr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cstemr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zstemr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstedc_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstedc_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cstedc_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zstedc_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstegr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstegr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cstegr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zstegr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall spteqr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dpteqr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cpteqr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zpteqr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sstebz_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstebz_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstein_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstein_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cstein_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zstein_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sdisna_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ddisna_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssygst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsygst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chegst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhegst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspgst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dspgst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chpgst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhpgst_ , Ptr CInt -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssbgst_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsbgst_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chbgst_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhbgst_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall spbstf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dpbstf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cpbstf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zpbstf_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgehrd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgehrd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgehrd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgehrd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorghr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorghr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sormhr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dormhr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunghr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunghr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cunmhr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zunmhr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgebal_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgebal_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgebal_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgebal_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgebak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgebak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgebak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgebak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall shseqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dhseqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chseqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhseqr_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall shsein_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dhsein_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chsein_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhsein_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall strevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtrevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctrevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strexc_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtrexc_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctrexc_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztrexc_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall strsen_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrsen_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctrsen_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztrsen_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall strsyl_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtrsyl_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctrsyl_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrsyl_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgghrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgghrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgghrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgghrd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggbal_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dggbal_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cggbal_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zggbal_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sggbak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggbak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggbak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggbak_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall shgeqz_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dhgeqz_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chgeqz_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhgeqz_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stgevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtgevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctgevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztgevc_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stgexc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtgexc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctgexc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztgexc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stgsen_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtgsen_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctgsen_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztgsen_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stgsyl_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtgsyl_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctgsyl_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztgsyl_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stgsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtgsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctgsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztgsna_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggsvp_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dggsvp_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cggsvp_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zggsvp_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stgsja_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtgsja_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctgsja_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztgsja_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgels_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgels_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgels_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgels_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgelsy_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgelsy_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgelsy_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgelsy_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgelss_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgelss_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgelss_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgelss_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgelsd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgelsd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgelsd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgelsd_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgglse_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgglse_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgglse_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgglse_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggglm_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggglm_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggglm_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggglm_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssyev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cheev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zheev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssyevd_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyevd_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cheevd_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zheevd_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssyevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cheevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zheevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssyevr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyevr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cheevr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zheevr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dspev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chpev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhpev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sspevd_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspevd_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chpevd_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhpevd_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chpevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhpevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssbev_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsbev_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chbev_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhbev_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssbevd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsbevd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chbevd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhbevd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssbevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsbevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chbevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhbevx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstev_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dstev_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sstevd_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstevd_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstevx_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstevx_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sstevr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dstevr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgees_ , CString -> CString -> <LAPACK_S_SELECT2> -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgees_ , CString -> CString -> <LAPACK_D_SELECT2> -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgees_ , CString -> CString -> <LAPACK_C_SELECT1> -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgees_ , CString -> CString -> <LAPACK_Z_SELECT1> -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgeesx_ , CString -> CString -> <LAPACK_S_SELECT2> -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeesx_ , CString -> CString -> <LAPACK_D_SELECT2> -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeesx_ , CString -> CString -> <LAPACK_C_SELECT1> -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeesx_ , CString -> CString -> <LAPACK_Z_SELECT1> -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgeev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgesvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgesvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgesvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgesvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgesdd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgesdd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgesdd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgesdd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgejsv_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgejsv_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgesvj_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgesvj_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggsvd_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggsvd_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggsvd_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggsvd_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssygv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsygv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chegv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhegv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssygvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsygvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chegvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhegvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssygvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsygvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chegvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhegvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspgv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dspgv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chpgv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhpgv_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sspgvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspgvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chpgvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhpgvd_ , Ptr CInt -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sspgvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dspgvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chpgvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhpgvx_ , Ptr CInt -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssbgv_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dsbgv_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall chbgv_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zhbgv_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ssbgvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsbgvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chbgvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhbgvd_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssbgvx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsbgvx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chbgvx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhbgvx_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgges_ , CString -> CString -> CString -> <LAPACK_S_SELECT3> -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgges_ , CString -> CString -> CString -> <LAPACK_D_SELECT3> -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgges_ , CString -> CString -> CString -> <LAPACK_C_SELECT2> -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgges_ , CString -> CString -> CString -> <LAPACK_Z_SELECT2> -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggesx_ , CString -> CString -> CString -> <LAPACK_S_SELECT3> -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggesx_ , CString -> CString -> CString -> <LAPACK_D_SELECT3> -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggesx_ , CString -> CString -> CString -> <LAPACK_C_SELECT2> -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggesx_ , CString -> CString -> CString -> <LAPACK_Z_SELECT2> -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sggev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggev_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zggev_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sggevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dggevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cggevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zggevx_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsfrk_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall ssfrk_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall zhfrk_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall chfrk_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall dtfsm_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stfsm_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztfsm_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctfsm_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtfttp_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stfttp_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztfttp_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctfttp_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtfttr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stfttr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztfttr_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctfttr_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtpttf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stpttf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztpttf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctpttf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtpttr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stpttr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztpttr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctpttr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtrttf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strttf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrttf_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctrttf_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtrttp_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall strttp_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztrttp_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctrttp_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall sgeqrfp_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqrfp_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqrfp_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeqrfp_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall clacgv_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlacgv_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slarnv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall dlarnv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall clarnv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall zlarnv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall sgeqr2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgeqr2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgeqr2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeqr2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slacpy_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlacpy_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clacpy_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlacpy_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgetf2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgetf2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgetf2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgetf2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall slaswp_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dlaswp_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall claswp_ , Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zlaswp_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall slange_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall dlange_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall clange_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall zlange_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall clanhe_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall zlanhe_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall slansy_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall dlansy_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall clansy_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall zlansy_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall slantr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall dlantr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall clantr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO CFloat
#ccall zlantr_ , CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO CDouble
#ccall slamch_ , CString -> IO CFloat
#ccall dlamch_ , CString -> IO CDouble
#ccall sgelq2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgelq2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgelq2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgelq2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slarfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlarfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clarfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlarfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slarfg_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall dlarfg_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall clarfg_ , Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall zlarfg_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall slarft_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlarft_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clarft_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlarft_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slarfx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall dlarfx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall clarfx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
#ccall zlarfx_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall slatms_ , Ptr CInt -> Ptr CInt -> CString -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlatms_ , Ptr CInt -> Ptr CInt -> CString -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clatms_ , Ptr CInt -> Ptr CInt -> CString -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> CString -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlatms_ , Ptr CInt -> Ptr CInt -> CString -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> CString -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slag2d_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dlag2s_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall clag2z_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zlag2c_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall slauum_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dlauum_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall clauum_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zlauum_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall slagge_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlagge_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clagge_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlagge_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slaset_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlaset_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall claset_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlaset_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slasrt_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlasrt_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall claghe_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlaghe_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slagsy_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dlagsy_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall clagsy_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zlagsy_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall slapmr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dlapmr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall clapmr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zlapmr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall slapy2_ , Ptr CFloat -> Ptr CFloat -> IO CFloat
#ccall dlapy2_ , Ptr CDouble -> Ptr CDouble -> IO CDouble
#ccall slapy3_ , Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CFloat
#ccall dlapy3_ , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble
#ccall slartgp_ , Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall dlartgp_ , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall slartgs_ , Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall dlartgs_ , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall cbbcsd_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cheswapr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetri2_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetri2x_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall chetrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall csyconv_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall csyswapr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csytri2_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csytri2x_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall csytrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall cunbdb_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cuncsd_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dbbcsd_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorbdb_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dorcsd_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsyconv_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dsyswapr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytri2_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytri2x_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dsytrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sbbcsd_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorbdb_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sorcsd_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssyconv_ , CString -> CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ssyswapr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssytri2_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssytri2x_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ssytrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zbbcsd_ , CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zheswapr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetri2_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetri2x_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zhetrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall zsyconv_ , CString -> CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall zsyswapr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsytri2_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsytri2x_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zsytrs2_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall zunbdb_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zuncsd_ , CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgemqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgemqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgemqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgemqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dgeqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall cgeqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zgeqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall sgeqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall sgeqrt3_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dgeqrt3_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall cgeqrt3_ , Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zgeqrt3_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stpmqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtpmqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctpmqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztpmqrt_ , CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall dtpqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctpqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztpqrt_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall stpqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall dtpqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ctpqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> IO ()
#ccall ztpqrt2_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
#ccall stprfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall dtprfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ctprfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall ztprfb_ , CString -> CString -> CString -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall csyr_ , CString -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> IO ()
#ccall zsyr_ , CString -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO ()
#ccall ilaver_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
