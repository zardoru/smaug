{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-
    Que lindo que me pida utilizar _dos_ pragmas
    para evitar tener que escribir los tipos.
-}
module Smaug.Lexer where
import Text.Parsec
import Text.Parsec.String
import Data.Char (isDigit, isAlpha, isAlphaNum)
import Control.Monad (void)

mspaces = many $ oneOf " \n\t\r";

lexeme x = do 
              mspaces
              v <- string x
              mspaces
              return v

decpart = do
            dot <- string "."
            rest <- many1 $ digit
            return (dot ++ rest)


number = do
          mspaces
          l <- many1 $ digit
          r <- option "" decpart
          mspaces
          return (l ++ r)
          
isIdentifierChar :: Char -> Bool
isIdentifierChar x = isAlphaNum x || x `elem` "._"
             
identifier = 
       do
           mspaces
           fc <- getFc
           rest <- getRest
           mspaces
           return (fc:rest)
       where
           getFc = oneOf "_" <|> satisfy isAlpha
           getRest = many $ satisfy isIdentifierChar

colons = lexeme "::"
eq = lexeme "="

lsbrace = lexeme "["
rsbrace = lexeme "]"
lparen = lexeme "("
rparen = lexeme ")"

reqspaces = do
    many1 $ satisfy (not . isAlpha) 

reserved x = do
    mspaces
    lex <- string x
    try $ lookAhead reqspaces
    return lex 

letRW = reserved "let"
numRW = reserved "num"
strRW = reserved "str"
mainRW = reserved "main"
whileRW = reserved "while"
ifRW = reserved "if"
forRW = reserved "for"
breakRW = reserved "break"
continueRW = reserved "continue"
returnRW = reserved "return"
elseRW = reserved "else"

{-
add = lexeme "+"
sub = lexeme "-"
mul = lexeme "*"
div = lexeme "/"
-}
semicolon = lexeme ";"