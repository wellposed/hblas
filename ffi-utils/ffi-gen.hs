{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Monad (mapM)
import Text.PrettyPrint.Mainland
import qualified Data.ByteString.Char8 as B
import qualified Language.C.Syntax as C
import qualified Language.C.Parser as P
import Data.Loc
import Language.Haskell.TH
import Foreign.Ptr
import Foreign.C.Types
import Data.Char

{-
note: does not yet conform to the ffi style of hOpenBLAS,
also needs language c quote to work


-}

-- | Parse the file in the argument, transform it, then pretty print it.
main :: IO ()
main = do
    args <- getArgs
    let fname = head args
    x <- parseFile fname
    writeFile (take (length fname - 1) fname ++ "hs")
              $ (++) required
              $ pprint
              $ x >>= transform fname

-- | Parses C headers
parseFile :: String -> IO [C.Definition]
parseFile filename = do
    let start = startPos filename
    let exts = []
    s <- B.readFile filename
    case P.parse exts [] P.parseUnit s start of
      Left err   -> error $ show err
      Right defs -> return defs

required :: String
required = "{-# LANGUAGE GeneralizedNewtypeDeriving #-}\nmodule Numerical.OpenBLAS.FFI where\nimport Foreign.Ptr\nimport Foreign.C.Types\nimport Data.Complex\n"

-- wow! 1-1 mapping
transform :: String -> C.Definition -> [Dec]
transform headerName (C.DecDef (C.InitGroup (C.DeclSpec _ _ retType _) _ [C.Init (C.Id functionName _) (C.Proto _ (C.Params argsDecl _ _) _) _ _ _ _] _) sd)
  = let ty = case argsDecl of
              [(C.Param _ (C.DeclSpec _ _ (C.Tvoid _) _) _ _)] -> (AppT (ConT (mkName "IO")) $ tyco retType)
              xs -> foldnconquer (AppT (ConT (mkName "IO")) $ tyco retType) $ paramify xs
        f = ForeignD
          $ ImportF CCall Unsafe (headerName ++ " " ++ functionName)
            (mkName functionName) ty
    in [f]
transform headerName (C.DecDef (C.TypedefGroup (C.DeclSpec _ _ (C.Tenum (Just (C.Id name _)) vals _ _) _) _ _ _) _)
  = let tname = name ++ "T"
        unname = "un" ++ tname
        fnName = "encode" ++ (caps . drop 1 . dropWhile (/= '_') $ name)
        nt = NewtypeD
             [] (mkName tname) []
             (RecC (mkName tname)
               [(mkName unname, NotStrict, ConT (mkName "CUChar"))])
             [mkName "Eq", mkName "Show"]
        da = DataD [] (mkName name) [] 
             (map (\(C.CEnum (C.Id n _) (Just (C.Const (C.IntConst _ _ _v _) _)) _) ->
               NormalC (mkName n) []) vals)
             [mkName "Eq", mkName "Show"]
        fty = SigD (mkName fnName)
              (AppT (AppT ArrowT (ConT (mkName name))) (ConT (mkName tname)))
        fun = FunD (mkName fnName) 
            $ map mkCls vals
        mkCls (C.CEnum (C.Id n _) (Just (C.Const (C.IntConst _ _ v _) _)) _)
            = Clause [ConP (mkName n) []] 
              (NormalB (AppE (ConE (mkName tname)) (LitE (IntegerL v))))
              []
    in [nt, da, fty, fun]


transform _ _ = []

foldnconquer :: Type -> [Type] -> Type
foldnconquer = foldr (\x y -> AppT (AppT ArrowT x) y)

paramify :: [C.Param] -> [Type]
paramify = map (\(C.Param _ (C.DeclSpec _ _ ty _) d _) -> tyco' d ty)

tyco' :: C.Decl -> C.TypeSpec -> Type
tyco' (C.Ptr _ d _) x = AppT (ConT (mkName "Ptr")) $ tyco' d x
tyco' _ x = tyco x

tyco :: C.TypeSpec -> Type
tyco (C.Tvoid _)               = ConT $ mkName "()"
tyco (C.Tint _ _)              = ConT $ mkName "CInt" -- TODO: the first arg is if it is signed
tyco (C.Tchar _ _)             = ConT $ mkName "CChar"
tyco (C.Tfloat _)              = ConT $ mkName "CFloat"
tyco (C.Tdouble _)             = ConT $ mkName "CDouble"
tyco (C.Tenum (Just (C.Id s _)) _ _ _)  = ConT $ mkName (s ++ "T")
tyco (C.Tnamed (C.Id "size_t" _) _ _)   = ConT $ mkName "CUInt"
tyco (C.Tnamed (C.Id "blasint" _) _ _)  = ConT $ mkName "CInt"
tyco (C.Tnamed (C.Id "openblas_complex_float" _) _ _)   = ConT $ mkName "(Ptr (Complex Float))"
tyco (C.Tnamed (C.Id "openblas_complex_double" _) _ _)  = ConT $ mkName "(Ptr (Complex Double))"
tyco x = error $ "tyco: unimplemented: " ++ show x

caps (x:xs) = toUpper x : map toLower xs