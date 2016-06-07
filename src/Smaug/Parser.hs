{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Smaug.Parser where
import Text.Parsec
import Smaug.Lexer
import Smaug.LangADT

-- de WYAH por Stephen Diehl
infixOp x f = do
       lexeme x
       return f

-- expresiones booleanas/numericas
smId       = do { x <- identifier; return $ Ident x }
smNum      = do { x <- number; return $ Lit $ read x }
smQuoteStr = do
    lexeme "\""
    str <- many $ noneOf "\""
    lexeme "\""
    return $ QuoteStr str
    
idOrNum    = try smId <|> smNum <|> smQuoteStr

expression = andor `chainl1` ex1
andor      = cmpop `chainl1` ex2
cmpop      = term `chainl1` ex3
term       = top `chainl1` ex4

parens     = do { lparen; expr <- expression; rparen; return $ Parens expr }   
top        = try call <|> try idOrNum <|> parens 

{-
    En orden de precedencia:
    && y ||
    <=, <, >=, >
    + y -
    * y /
-}
ex1 = (infixOp "&&" And) <|> (infixOp "||" Or)
ex2 = try (infixOp "<=" Leq) <|> -- lookahead intencional
      try (infixOp ">=" Geq) <|> 
      (infixOp ">" Gt) <|>
      (infixOp "<" Lt) <|>
      (infixOp "==" Eq) <|>
      (infixOp "!=" Neq)

ex3 = (infixOp "+" Add) <|> (infixOp "-" Sub)
ex4 = (infixOp "*" Mul) <|> (infixOp "/" Div)

-- letStmt := "let" id '=' expr ';'
letStmt = do
    letRW
    id <- identifier
    eq
    expr <- expression
    semicolon
    return $ LetExpr id expr
       
-- assnStmt := id '=' expr ';'     
assnStmt = do
    id <- identifier
    eq
    expr <- expression
    semicolon
    return $ AssnExpr id expr

-- whileStmt := "while" '(' expr ')' body
whileStmt = do
    whileRW
    expr <- expression
    bd <- bodyOrStmt
    return $ WhileExpr expr bd
    
-- ifStmt := "if" '(' expr ')' body
ifStmt = do
    ifRW
    expr <- expression
    body <- bodyOrStmt
    maybeElse <- optionMaybe (do {elseRW; bodyOrStmt})
    case maybeElse of
        Just elseBody -> return $ IfExpr expr body elseBody
        Nothing -> return $ IfExpr expr body []

-- maneja mas de 1 parametro mediante sepBy!
-- paramList := expr (',' expr)*
paramList = expression `sepBy` (lexeme ",")

-- call := ident '(' (paramList)? ')'
call = do
    id <- identifier
    lparen
    exprList <- paramList
    rparen
    return $ FunCallExpr id exprList

-- callStmt := call ';'
callStmt = do
    fcall <- call
    semicolon
    return $ BodyCallExpr fcall 

breakStmt = do
    breakRW
    semicolon
    return BreakStmt

returnStmt = do
    returnRW
    semicolon
    return ReturnStmt

continueStmt = do
    continueRW
    semicolon
    return ContinueStmt

forStmt = do
    lexeme "for"
    oparen <- option "" (lexeme "(") 
    (LetExpr ident xpr) <- letStmt 
    dest <- number
    semicolon
    step <- option "1" number
    case oparen of 
        "(" -> lexeme ")"
        _ -> mspaces
    bd <- bodyOrStmt
    return $ ForExpr ident xpr (read dest) (read step) bd

-- stmt := letStmt | whileStmt | ifStmt | 
-- assnStmt | callStmt | breakStmt | 
-- continueStmt | returnStmt
stmt = do
    statement <- try letStmt <|> 
                 try whileStmt <|> 
                 try ifStmt <|>
                 try forStmt <|>
                 try breakStmt <|>
                 try returnStmt <|>
                 try continueStmt <|>
                 try assnStmt <|>
                 callStmt
    return statement

-- body := '[' stmt* ']' | stmt 
body = do
    lsbrace 
    bd <- many $ stmt
    rsbrace
    return bd
    
stmtBody = do
    bd <- stmt 
    return [bd]
    
bodyOrStmt = body <|> stmtBody 

-- De acá obtenemos el arbol.
{-
    "main" "::" body
-}
smaugParser :: Stream s m Char => ParsecT s u m BodyExpr
smaugParser = do
                mainRW
                colons
                bd <- body
                return $ MainBody bd
     
-- Árbol programable
smaugParse = parse smaugParser ""

-- Statements
smaugParseStmt = parse stmt ""

-- Expresiones simples
smaugParseExpr = parse expression ""