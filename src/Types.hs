{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Dynamic
import Data.List.NonEmpty as NL
import qualified Data.Map as M
import Data.List (intercalate, foldl')

data RtProc =
  PrimProc String Int |
  Closure [String] (NL.NonEmpty LispAst) Env deriving (Generic, Typeable)

data RtType =
  Unit |
  Numb Int |
  Str String |
  Bl Bool |
  Proc RtProc deriving (Eq, Generic, Show, Typeable)

primitiveToRuntimeType :: PrimitiveAst -> RtType
primitiveToRuntimeType UnitAst = Unit
primitiveToRuntimeType (NumbAst n) = Numb n
primitiveToRuntimeType (StrAst s) = Str s
primitiveToRuntimeType (BlAst b) = Bl b

data PrimitiveAst =
  UnitAst |
  NumbAst Int |
  StrAst String |
  BlAst Bool deriving (Eq, Generic, Show, Typeable)

data SpecialFormAst =
  Define String LispAst |
  DefineProc String [String] (NL.NonEmpty LispAst) |
  Assign String LispAst |
  Begin (NL.NonEmpty LispAst) |
  Let (NL.NonEmpty (String, LispAst)) (NL.NonEmpty LispAst) |
  Lambda [String] (NL.NonEmpty LispAst) |
  Cond (NonEmpty (LispAst, LispAst)) (Maybe LispAst) |
  If LispAst LispAst LispAst deriving (Eq, Generic, Show, Typeable)

data LispAst =
  Const PrimitiveAst |
  Var String |
  Sf SpecialFormAst |
  App LispAst [LispAst] deriving (Eq, Generic, Show, Typeable)

data PrimProcApplyError =
  ProcedureNotFound String |
  InvalidTypesPassed String |
  InvalidProcedure String |
  FailedToApplyArg Int String

newtype Env = Env [M.Map String RtType] deriving (Eq, Generic, Typeable)

data ApplyError = NoDefProcedure String Env |
  ProcApplyError String |
  IncorrectNumOfArgs String Int Int |
  PrimProcApplyErr PrimProcApplyError
data EvalError = VarNotFound String Env |
  IncorrectCondType |
  IncorrectNumOfConditions |
  InvalidOperatorType RtType |
  ApError ApplyError

instance Show Env where
  show (Env e) = foldl' (\s env -> ("|" <> intercalate ", " (M.keys env) <> "|") ++ s) "" e

instance Show RtProc where
  show Closure {} = "<procedure>"
  show (PrimProc name _) =  "<" <> name <> "-primitive-procedure>"

instance Eq RtProc where
  Closure {} == Closure {} = False
  (PrimProc nm1 argNum1) == (PrimProc nm2 argNum2) = nm1 == nm2 && argNum1 == argNum2

instance Show EvalError where
  show (VarNotFound x env) = "Failed to find variable '" <> x <> "' in the current env " <> show env
  show IncorrectCondType = "Provided condition is not of the type boolean"
  show (InvalidOperatorType pt) = "Operator name has invalid type " <> show pt
  show IncorrectNumOfConditions = "Cond operator is provided with no else clause end less then two conditional sections"
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
