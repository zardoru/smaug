module Smaug.CodeGen where
import System.IO
import Data.List (foldl', concat, intercalate)

import Smaug.LangADT

{-
     Compila a Javascript!
-}

generateCode' :: [BodyExpr] -> String
generateCode' = concat . map generateCode 

infixGenerate op l r =
    generateCodeEx l ++ op ++ generateCodeEx r
    
generateCodeEx :: Expr -> String
generateCodeEx (Add l r) = infixGenerate "+" l r
generateCodeEx (Mul l r) = infixGenerate "*" l r
generateCodeEx (Sub l r) = infixGenerate "-" l r
generateCodeEx (Div l r) = infixGenerate "/" l r
generateCodeEx (Lt l r) = infixGenerate "<" l r
generateCodeEx (Gt l r) = infixGenerate ">" l r
generateCodeEx (Leq l r) = infixGenerate "<=" l r
generateCodeEx (Geq l r) = infixGenerate ">=" l r
generateCodeEx (Neq l r) = infixGenerate "!=" l r
generateCodeEx (Eq l r) = infixGenerate "===" l r
generateCodeEx (And l r) = infixGenerate "&&" l r
generateCodeEx (Or l r) = infixGenerate "||" l r
generateCodeEx (Ident s) = s
generateCodeEx (Lit d) = show d
generateCodeEx (QuoteStr s) = "\"" ++ s ++ "\""
generateCodeEx (FunCallExpr s x) = s ++ 
                    "(" ++ (intercalate "," $ map generateCodeEx x) ++ ")"
generateCodeEx x = error $ "Unimplemented expr codegen path " ++ show x

generateCode :: BodyExpr -> String
generateCode expr = 
    case expr of
        (MainBody lxpr) -> generateCode' lxpr
        (LetExpr i xpr) -> "var " ++ i ++ "=" ++ generateCodeEx xpr ++ ";"
        (AssnExpr i xpr) -> i ++ "=" ++ generateCodeEx xpr ++ ";"
        (WhileExpr xpr bxpr) -> "while(" ++ 
                                generateCodeEx xpr ++ "){" ++ 
                                generateCode' bxpr ++ "}"
        (IfExpr xpr bxpr) -> "if(" ++
                              generateCodeEx xpr ++ "){" ++
                              generateCode' bxpr ++ "}"
        (BodyCallExpr xpr) -> generateCodeEx xpr ++ ";"
        _ -> error $ "Unimplemented codegen path " ++ show expr

smaugGenerateCode :: FilePath -> BodyExpr -> IO ()
smaugGenerateCode p expr = do
    let strOut = generateCode expr
    writeFile p strOut