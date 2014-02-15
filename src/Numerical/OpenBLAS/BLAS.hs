
module Numerical.OpenBLAS.BLAS where
--(
--    dgemm
--    ,sgemm
--    ,cgemm
--    ,zgemm) 


import Numerical.OpenBLAS.UtilsFFI    
import Numerical.OpenBLAS.BLAS.FFI 
import Numerical.OpenBLAS.MatrixTypes
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as SM

---- |  Matrix mult for general dense matrices
--type GemmFunFFI scale el = CBLAS_ORDERT -> CBLAS_TRANSPOSET ->  CBLAS_TRANSPOSET -> CBLAS_TRANSPOSET->
        --CInt -> CInt -> CInt -> scale -> Ptr el  -> CInt -> Ptr el -> scale -> CInt -> Ptr el -> IO ()
--type GemmFun = MutDenseMatrix or el ->  MutDenseMatrix or el ->   MutDenseMatrix or el -> 

--sgemm 
-- for c += A * B
-- we have  cy ==  ay , cx == bx , ax == by
--gemmAbstraction :: 


--isGoodGemDims 

flopsThreshold = 10000
gemmComplexity a b c = a * b * c  -- this will be wrong by some constant factor, albeit a small one


-- this covers the ~6 cases for checking the dimensions for GEMM quite nicely
isBadGemm tra trb trc ax ay bx by cx cy = isBadGemmHelper (cds tra (ax,ay)) (cds trb (bx,by) ) (cds trc (cx,cy))
    cds = coordSwapper 
    coordSwapper NoTranpose (a,b) = (a,b)
    coordSwapper ConjNoTranpose (a,b) = (a,b) 
    coordSwapper Tranpose (a,b) = (b,a)
    coordSwapper ConjTranpose (a,b) = (b,a)
    isBadGemmHelper !(ax,ay) !(bx,by) !(cx,cy) =  (minimum [ax, ay, bx, by, cx ,cy] <= 0) || not (  cy ==  ay && cx == bx && ax == by)

encodeFFITranpose  x=  encodeTranpose $ encodeNiceTranpose x 
encodeNiceTranpose x = case x of 
        NoTranpose -> BlasNoTranspose
        Tranpose -> BlasTranpose
        ConjTranpose -> BlasConjTranpose
        ConjNoTranpose -> BlasConjNoTranpose

--data BLAS_Tranpose = BlasNoTranspose | BlasTranpose | BlasConjTranspose | BlasConjNoTranpose 
--data Tranpose = NoTranpose | Tranpose | ConjTranpose | ConjNoTranpose

gemmAbstraction gemmSafeFFI gemmUnsafeFFI handler = go 
  where 
    go  tra trb trc alpha beta 
        a@(RowMajorMutableDenseMatrix ax ay astride abuff) 
        b@(RowMajorMutableDenseMatrix  bx by bstride bbuff) 
        c@(RowMajorMutableDenseMatrix  cx cy cstride cbuff) 
            |  isBadGemm tra trb trc ax ay bx by cx cy = error $! "bad dimension args to GEMM: ax ay bx by cx cy: " ++ show [ax, ay, bx, by, cx ,cy]
            | flopsThreshold >= gemmComplexity cy cx ax  = do 
                ap<- unsafeWithPrim abuff
                bp <- unsafeWithPrim bbuff
                cp <- unsafeWithPrim cbuff 

    go  tra trb trc alpha beta 
        a@(ColMajorMutableDenseMatrix ax ay astride abuff) 
        b@(ColMajorMutableDenseMatrix  bx by bstride bbuff) 
        c@(ColMajorMutableDenseMatrix  cx cy cstride cbuff)
            | isBadGemm tra trb trc ax ay bx by cx cy = error $! "bad dimension args to GEMM: ax ay bx by cx cy: " ++ show [ax, ay, bx, by, cx ,cy]
            | flopsThreshold >= gemmComplexity cy cx ax  = -- call safe gemm
            | otherwise = --call unsafe gemm, small enough input that ffi overhead matters 



