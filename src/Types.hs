{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Dynamic
import Data.List.NonEmpty as NL
import qualified Data.Map as M

data PrimitiveType = Unit |
  Numb Integer |
  Str String |
  Bl Bool |
  Lambda [String] LispAst deriving (Eq, Generic, Show, Typeable)

data SpecialForm =
  Define String LispAst |
  DefineProc String [String] LispAst |
  Assign String LispAst |
  Begin (NL.NonEmpty LispAst) |
  If LispAst LispAst LispAst deriving (Eq, Generic, Show, Typeable)

data LispAst =
  Const PrimitiveType |
  Var String |
  Sf SpecialForm |
  App LispAst [LispAst] deriving (Eq, Generic, Show, Typeable)

data PrimProcApplyError = 
  ProcedureNotFound String |
  InvalidTypesPassed String |
  InvalidProcedure String |
  FailedToApplyArg Int String

instance Show PrimProcApplyError where
  show (ProcedureNotFound opName) = "No primitive operation called " <> opName
  show (InvalidTypesPassed opName) = "Invalid primitive types passed to " <> opName
  show (InvalidProcedure opName) = "Failed to apply primitive procedure" <> opName
  show (FailedToApplyArg ind opName) = "Failed to apply " <> show ind <> "th argument in" <> opName
