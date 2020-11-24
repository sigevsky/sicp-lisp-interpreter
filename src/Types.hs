{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Dynamic
import Data.List.NonEmpty as NL
import qualified Data.Map as M
import Data.IORef
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

primitiveSyntaxToAst :: PrimitiveSyntax -> PrimitiveAst
primitiveSyntaxToAst UnitS = UnitAst
primitiveSyntaxToAst (NumbS n) = NumbAst n
primitiveSyntaxToAst (StrS s) = StrAst s
primitiveSyntaxToAst (BlS b) = BlAst b

data PrimitiveAst =
  UnitAst |
  NumbAst Int |
  StrAst String |
  BlAst Bool deriving (Eq, Generic, Show, Typeable)

data LispAst =
  Const PrimitiveAst |
  Var String |
  Define String LispAst |
  Assign String LispAst |
  Begin (NL.NonEmpty LispAst) |
  Lambda [String] (NL.NonEmpty LispAst) |
  If LispAst LispAst LispAst |
  App LispAst [LispAst] deriving (Eq, Generic, Show, Typeable)

data PrimitiveSyntax =
  UnitS |
  NumbS Int |
  StrS String |
  BlS Bool deriving (Eq, Generic, Show, Typeable)

data SpecialFormSyntax =
  DefineS String LispSyntax |
  DefineProcS String [String] (NL.NonEmpty LispSyntax) |
  AssignS String LispSyntax |
  BeginS (NL.NonEmpty LispSyntax) |
  LetS (NL.NonEmpty (String, LispSyntax)) (NL.NonEmpty LispSyntax) |
  LetAsteriskS (NL.NonEmpty (String, LispSyntax)) (NL.NonEmpty LispSyntax) |
  NamedLetS String (NL.NonEmpty (String, LispSyntax)) (NL.NonEmpty LispSyntax) |
  LambdaS [String] (NL.NonEmpty LispSyntax) |
  CondS (NonEmpty (LispSyntax, LispSyntax)) (Maybe LispSyntax) |
  IfS LispSyntax LispSyntax LispSyntax |
  AndS (NonEmpty LispSyntax) |
  OrS (NonEmpty LispSyntax) deriving (Eq, Generic, Show, Typeable)

data LispSyntax =
  ConstS PrimitiveSyntax |
  VarS String |
  SfSyntax SpecialFormSyntax |
  AppS LispSyntax [LispSyntax] deriving (Eq, Generic, Show, Typeable)

data PrimProcApplyError =
  ProcedureNotFound String |
  InvalidTypesPassed String |
  InvalidProcedure String |
  FailedToApplyArg Int String

type Frame = M.Map String RtType
newtype Env = Env [IORef Frame] deriving (Eq, Generic, Typeable)

newtype FixedEnv = FixedEnv [Frame]

data ApplyError = NoDefProcedure String FixedEnv |
  ProcApplyError String |
  IncorrectNumOfArgs String Int Int |
  PrimProcApplyErr PrimProcApplyError
data EvalError = VarNotFound String FixedEnv |
  IncorrectCondType |
  IncorrectNumOfConditions |
  InvalidOperatorType RtType |
  AccessingUninitializedVar String |
  ApError ApplyError |
  EmptyEnvAccess 

isPrimitiveProcVar :: RtType -> Bool
isPrimitiveProcVar (Proc (PrimProc _ _)) = True
isPrimitiveProcVar _                     = False

instance Show FixedEnv where
  show (FixedEnv e) = foldl' (\s env -> ("|" <> intercalate ", " (M.keys env) <> "|") ++ s) "" e
    where vars env = fmap show . Prelude.filter (not . isPrimitiveProcVar . snd) $ Prelude.zip (M.keys env) (M.elems env)

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
  show (AccessingUninitializedVar varName) = "Accessing not yet initialized variable " <> varName
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
