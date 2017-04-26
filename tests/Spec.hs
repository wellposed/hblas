--import qualified HBLAS.BLAS.Level1Spec
import qualified HBLAS.BLAS.Level2Spec
import qualified HBLAS.BLAS.Level3Spec
import qualified HBLAS.MatrixTypesSpec

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  --describe "HBLAS.BLAS.Level1" HBLAS.BLAS.Level1Spec.spec
  describe "HBLAS.BLAS.Level2" HBLAS.BLAS.Level2Spec.spec
  describe "HBLAS.BLAS.Level3" HBLAS.BLAS.Level3Spec.spec
  describe "HBLAS.MatrixTypes" HBLAS.MatrixTypesSpec.spec
