module Test where

import Types
import LispEval
import Text.Megaparsec
import Text.Megaparsec.Error
import LispParser
import Shower
import GHC.TypeNats
import ExprExpander (expandSyntax)

testStr parser str = case runParser parser "blah" str of
             Right v -> printer v
             Left v -> putStrLn $ errorBundlePretty v

test = testStr lispAstP

parseFile fname = readFile ("examples/" <> fname) >>= testStr lispFileP

evalFile fname = do
    fdata <- readFile ("examples/" <> fname)
    case runParser lispFileP "blah" fdata of
       Right v -> evaluate (expandSyntax v) >>= print
       Left v -> putStrLn $ errorBundlePretty v

expandFile fname = do
     fdata <- readFile ("examples/" <> fname)
     case runParser lispFileP "blah" fdata of
        Right v -> printer (expandSyntax v)
        Left v -> putStrLn $ errorBundlePretty v
