import System.IO (openFile, hGetContents, IOMode(ReadMode))
import System.Environment (getArgs)
import Control.Monad (forever)

import Smaug.Lexer
import Smaug.Parser
import Smaug.MathEval
import Smaug.TreeParser

parseFile :: FilePath -> IO ()
parseFile x = do 
                handle <- openFile x ReadMode
                cnts <- hGetContents handle
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
        handle <- openFile x ReadMode
        cnts <- hGetContents handle
        let msg = smaugParse cnts
            (Right expr) = msg
        print $ smaugCreateSymbolTable expr
        
parseStmt :: IO ()
parseStmt = do
    line <- getLine
    let out = smaugParseStmt line
    print $ parseResult out

main :: IO ()
main = do
        x <- getArgs
        if length x > 0 then
                if x !! 0 == "-i"
                    then forever parseStmt
                    else parseFile $ x !! 0 
            else forever parseStmt