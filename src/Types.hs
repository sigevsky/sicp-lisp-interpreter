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

newtype Env = Env [M.Map String PrimitiveType]

data ApplyError = NoDefProcedure String Env |
  ProcApplyError String |
  IncorrectNumOfArgs String Int Int |
  PrimProcApplyErr PrimProcApplyError
data EvalError = VarNotFound String Env |
  IncorrectCondType |
  InvalidOperatorType PrimitiveType |
  ApError ApplyError

instance Show EvalError where
  show (VarNotFound x env) = "Failed to find variable " <> x <> " in the current env"
  show IncorrectCondType = "Provided condition is not of the type boolean"
  show (InvalidOperatorType pt) = "Operator name has invalid type " <> show pt
  show (ApError e) = show e

instance Show ApplyError where
  show (ProcApplyError opName) = opName <> " is not a procedure"
  show (NoDefProcedure opName env) = "No procedure " <> opName <> " defined in the current scope"
  show (IncorrectNumOfArgs opName passed required) = "Incorrect number of arguments passed to " <> opName <> " " <> show passed <> " instead of " <> show required
  show (PrimProcApplyErr s) = show s

instance Show PrimProcApplyError where
  show (ProcedureNotFound opName) = "No primitive operation called " <> opName
  show (InvalidTypesPassed opName) = "Invalid primitive types passed to " <> opName
  show (InvalidProcedure opName) = "Failed to apply primitive procedure" <> opName
  show (FailedToApplyArg ind opName) = "Failed to apply " <> show ind <> "th argument in" <> opName
