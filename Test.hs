import Numerical.OpenBLAS.FFI

import Foreign.C.Types

main :: IO ()
main = do
  -- Just test that the symbol resolves
  openblas_set_num_threads_ffi 1
