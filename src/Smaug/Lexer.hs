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
             
identifier = 
       do
           mspaces
           fc <- getFc
           rest <- getRest
           mspaces
           return (fc:rest)
       where
           getFc = oneOf "_$@" <|> satisfy isAlpha
           getRest = many $ satisfy isAlphaNum

colons = lexeme "::"
eq = lexeme "="

lsbrace = lexeme "["
rsbrace = lexeme "]"
lparen = lexeme "("
rparen = lexeme ")"

letRW = lexeme "let"
numRW = lexeme "num"
strRW = lexeme "str"
mainRW = lexeme "main"
whileRW = lexeme "while"
ifRW = lexeme "if"
forRW = lexeme "for"

{-
add = lexeme "+"
sub = lexeme "-"
mul = lexeme "*"
div = lexeme "/"
-}
semicolon = lexeme ";"