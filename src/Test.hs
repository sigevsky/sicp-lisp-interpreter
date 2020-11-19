module Test where

import Types
import LispEval
import Text.Megaparsec
import Text.Megaparsec.Error
import LispParser
import Shower


test str = case runParser lispAstP "blah" str of
             Right v -> printer v
             Left v -> putStrLn $ errorBundlePretty v

