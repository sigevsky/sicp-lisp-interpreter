{-# LANGUAGE OverloadedStrings #-}
module LispParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Text as T
import Types
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.List.NonEmpty as NE (NonEmpty(..), head, tail)

reservedWords = ["define", "lambda", "begin", "set!"]

allowedSymbols :: Parser Char
allowedSymbols = choice [char '&', char '*', char '/', char '-']

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

parens = between (symbol "(") (symbol ")")

name :: Parser String
name = lexeme $ do
     nm <- some (alphaNumChar <|> symbolChar <|> allowedSymbols)
     if nm `elem` reservedWords
       then fail $ nm <> " is a reserved word"
       else return nm

args :: Parser [String]
args = parens (many name)

primitiveTypeP :: Parser PrimitiveType
primitiveTypeP = choice [
    Unit <$ symbol "Unit",
    Numb <$> lexeme L.decimal,
    Bl <$> lexeme boolP,
    lambdaP
  ]

boolP :: Parser Bool
boolP = True <$ symbol "#t" <|> False <$ symbol "#f"

lambdaP :: Parser PrimitiveType
lambdaP = parens (Lambda <$> (symbol "lambda" *> args) <*> lispAstP)


specialFormP :: Parser SpecialForm
specialFormP = choice [
    try . parens $ defineP,
    try . parens $ defineProcP,
    try . parens $ ifP,
    try . parens $ assignP,
    try . parens $ letP,
    parens beginP
  ]

defineP :: Parser SpecialForm
defineP = symbol "define" *> ps
  where ps = Define <$> name <*> lispAstP

defineProcP :: Parser SpecialForm
defineProcP = symbol "define" *> ps
  where
    ps = do
      fname NE.:| args <- parens (NE.some name)
      DefineProc fname args <$> lispAstP

assignP :: Parser SpecialForm
assignP = symbol "set!" *> ps
  where ps = Assign <$> name <*> lispAstP

beginP :: Parser SpecialForm
beginP = symbol "begin" *> ps
  where ps = Begin <$> NE.some lispAstP

ifP :: Parser SpecialForm
ifP = symbol "if" *> ps
 where ps = If <$> lispAstP <*> lispAstP <*> lispAstP
 
letP :: Parser SpecialForm
letP = symbol "let" *> ps
  where ps = Let <$> argPairs <*> lispAstP
        argPairs = parens (NE.some argPair)
        argPair = parens ((,) <$> name <*> lispAstP)

lispAstP :: Parser LispAst
lispAstP = lexeme ps
  where
    appP :: Parser LispAst
    appP = parens (App <$> (fmap Var (try name) <|> lispAstP) <*> many lispAstP)
    ps :: Parser LispAst
    ps = choice [
        try $ Sf <$> specialFormP,
        try $ Const <$> primitiveTypeP,
        try $ Var <$> lexeme name,
        appP
      ]
