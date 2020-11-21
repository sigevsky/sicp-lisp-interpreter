{-# LANGUAGE OverloadedStrings #-}
module LispParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Text as T
import Types (LispSyntax (..), SpecialFormSyntax(..), PrimitiveSyntax (..))
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.List.NonEmpty as NE (NonEmpty(..), head, tail)

reservedWords = ["define", "lambda", "begin",
                 "set!", "cond", "else",
                 "let", "let*", "and", "or"]

allowedSymbols :: Parser Char
allowedSymbols = choice $ char <$> ['&', '*', '/', '-', '?', '!', '\'']

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space (space1 <|> skipSome newline <|> skipSome tab)
                        (L.skipLineComment ";")
                        (L.skipBlockComment "#|" "|#")

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

procBody = NE.some lispAstP

primitiveTypeP :: Parser PrimitiveSyntax
primitiveTypeP = choice [
    UnitS <$ symbol "Unit",
    NumbS <$> lexeme L.decimal,
    StrS <$> lexeme stringP,
    BlS <$> lexeme boolP
  ]

stringP :: Parser String
stringP = between (symbol "\"") (symbol "\"") (many (spaceChar <|> alphaNumChar <|> symbolChar <|> allowedSymbols))

boolP :: Parser Bool
boolP = True <$ symbol "#t" <|> False <$ symbol "#f"

lambdaP :: Parser SpecialFormSyntax
lambdaP = LambdaS <$> (symbol "lambda" *> args) <*> procBody


specialFormP :: Parser SpecialFormSyntax
specialFormP = choice [
    try . parens $ defineP,
    try . parens $ defineProcP,
    try . parens $ lambdaP,
    try . parens $ ifP,
    try . parens $ assignP,
    try . parens $ letP,
    try . parens $ condP,
    try . parens $ andP,
    try . parens $ orP,
    parens beginP
  ]

defineP :: Parser SpecialFormSyntax
defineP = symbol "define" *> ps
  where ps = DefineS <$> name <*> lispAstP

defineProcP :: Parser SpecialFormSyntax
defineProcP = symbol "define" *> ps
  where
    ps = do
      fname NE.:| args <- parens (NE.some name)
      DefineProcS fname args <$> procBody

assignP :: Parser SpecialFormSyntax
assignP = symbol "set!" *> ps
  where ps = AssignS <$> name <*> lispAstP

beginP :: Parser SpecialFormSyntax
beginP = symbol "begin" *> ps
  where ps = BeginS <$> procBody

ifP :: Parser SpecialFormSyntax
ifP = symbol "if" *> ps
  where ps = IfS <$> lispAstP <*> lispAstP <*> lispAstP

andP :: Parser SpecialFormSyntax
andP = symbol "and" *> ps
  where ps = AndS <$> NE.some lispAstP

orP :: Parser SpecialFormSyntax
orP = symbol "or" *> ps
  where ps = OrS <$> NE.some lispAstP

condP :: Parser SpecialFormSyntax
condP = symbol "cond" *> ps
  where conds = NE.some cond
        cond = try $ parens ((,) <$> lispAstP <*> lispAstP)
        els = optional (parens $ symbol "else" *> lispAstP)
        ps = CondS <$> conds <*> els

letP :: Parser SpecialFormSyntax
letP = try (symbol "let*" *> lta) <|> symbol "let" *> lt
  where lt = LetS <$> argPairs <*> procBody
        lta = LetAsteriskS <$> argPairs <*> procBody
        argPairs = parens (NE.some argPair)
        argPair = parens ((,) <$> name <*> lispAstP)

lispAstP :: Parser LispSyntax
lispAstP = lexeme ps
  where
    appP :: Parser LispSyntax
    appP = parens (AppS <$> (fmap VarS (try name) <|> lispAstP) <*> many lispAstP)
    ps :: Parser LispSyntax
    ps = choice [
        try $ SfSyntax <$> specialFormP,
        try $ ConstS <$> primitiveTypeP,
        try $ VarS <$> lexeme name,
        appP
      ]

lispFileP :: Parser LispSyntax
lispFileP = spaceConsumer *> (SfSyntax . BeginS <$> NE.some lispAstP)
