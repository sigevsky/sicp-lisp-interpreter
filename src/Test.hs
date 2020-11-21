{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Test where

import Types
import LispEval
import Text.Megaparsec
import Text.Megaparsec.Error
import LispParser
import Shower
import GHC.TypeNats
import ExprExpander (expand)

testStr parser str = case runParser parser "blah" str of
             Right v -> printer v
             Left v -> putStrLn $ errorBundlePretty v

test = testStr lispAstP

parseFile fname = readFile ("examples/" <> fname) >>= testStr lispFileP

evalFile fname = do
    fdata <- readFile ("examples/" <> fname)
    case runParser lispFileP "blah" fdata of
       Right v -> evaluate (expand v) >>= print
       Left v -> putStrLn $ errorBundlePretty v
