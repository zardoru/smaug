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

parens     = do { lparen; expr <- expression; rparen; return expr }   
top        = try call <|> idOrNum <|> parens 

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

-- whileStmt := "while" '('expr')' body
whileStmt = do
    whileRW
    lparen
    expr <- expression
    rparen
    bd <- bodyOrStmt
    return $ WhileExpr expr bd
    
-- ifStmt := "if" '('expr')' body
ifStmt = do
    ifRW
    lparen
    expr <- expression
    rparen
    body <- bodyOrStmt
    return $ IfExpr expr body

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

-- stmt := letStmt | whileStmt | ifStmt | assnStmt | callStmt
stmt = do
    statement <- try letStmt <|> 
                 try whileStmt <|> 
                 try ifStmt <|> 
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
    
bodyOrStmt = try body <|> stmtBody 

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