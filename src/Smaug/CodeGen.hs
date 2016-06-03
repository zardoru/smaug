module Smaug.CodeGen where
import System.IO
import Data.List (foldl', concat, intercalate)

import Smaug.LangADT

{-
     Compila a Javascript!
-}

generateCode' :: [BodyExpr] -> String
generateCode' = concat . map generateCode 

generateCode :: BodyExpr -> String
generateCode expr = 
    case expr of
        (MainBody lxpr) -> generateCode' lxpr
        _ -> error "Unimplemented codegen path."

smaugGenerateCode :: FilePath -> BodyExpr -> IO ()
smaugGenerateCode p expr = do
    let strOut = generateCode expr
    writeFile p strOut