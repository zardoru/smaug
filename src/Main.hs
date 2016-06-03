import System.IO
import System.Environment (getArgs)
import Control.Monad (forever)

import Smaug.LangADT
import Smaug.Parser
import Smaug.SemanticRules.SymbolsAndScope
import Smaug.TreeParser
import Smaug.CodeGen

parseFile :: FilePath -> IO ()
parseFile x = do 
                cnts <- readFile x
                let msg = smaugParse cnts
                print msg

parseResult :: Show a => Either a BodyExpr -> String
parseResult (Left x) = show x
parseResult (Right body) = 
    case status of
        Ok -> show body
        SemanticError x -> x
    where
        status = smaugCheckSemanticRules body
        
printSymbolTable x =
    do
        cnts <- readFile x
        let msg = smaugParse cnts
            (Right expr) = msg
        print $ smaugCreateSymbolTable expr
        
semanticCheckFile x = 
    do
        cnts <- readFile x
        let msg = smaugParse cnts
            (Right expr) = msg
        print $ smaugCheckSemanticRules expr
        
parseStmt :: IO ()
parseStmt = do
    putStr "smaug> "
    line <- getLine
    let out = smaugParseStmt line
    print $ parseResult out
   
genCodeFromSmaugFile :: String -> String -> IO()
genCodeFromSmaugFile x dst =
    do
        putStrLn $ "Parsing from file " ++ x ++ "..."
        cnts <- readFile x 
        let msg = smaugParse cnts
        case msg of
            (Left err) -> hPutStrLn stderr $ show err
            (Right code) -> genStageTwo code dst 

genStageTwo :: BodyExpr -> String -> IO()
genStageTwo code dst =
    do
        putStrLn "Performing semantic check..."
        case smaugCheckSemanticRules code of
            Ok -> genStageThree code dst
            (SemanticError err) -> hPutStrLn stderr $ "Semantic error: " ++ err

genStageThree :: BodyExpr -> String -> IO()
genStageThree code dst = smaugGenerateCode dst code
        
invokeCompiler :: [String] -> IO ()
invokeCompiler args =
    case length args of
        2 -> error "Missing argument (output file)"
        1 -> error "Missing arguments (input and output file)"
        3 -> genCodeFromSmaugFile (args !! 1) (args !! 2)

main :: IO ()
main = do
        args <- getArgs
        if length args > 0 then
            case args !! 0 of
                "-i" -> forever parseStmt
                "-c" -> invokeCompiler args  
            else forever parseStmt