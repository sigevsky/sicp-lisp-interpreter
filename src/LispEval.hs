{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module LispEval where

import GHC.Generics
import qualified Data.Map as M
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (foldl')
import Control.Monad
import Control.Monad.Except
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Cont (MonadCont)
import Control.Monad.State (MonadState, StateT (..), get, put)
import Control.Monad.Writer (MonadWriter)
import Data.List.NonEmpty as NL
import Data.Dynamic

data PrimitiveType = Unit |
  Numb Integer |
  Str String |
  Bl Bool deriving (Eq, Generic, Show)

data BaseType = Primitive PrimitiveType | Lambda [String] LispAst deriving (Eq, Generic, Show)

data SpecialForm =
  Define String LispAst |
  DefineProc String [String] LispAst |
  Assign String LispAst |
  Begin (NL.NonEmpty LispAst) |
  If LispAst LispAst LispAst deriving (Eq, Generic, Show)

data LispAst =
  Const BaseType |
  Var String |
  Sf SpecialForm |
  App LispAst [LispAst] deriving (Eq, Generic, Show)


newtype Env = Env [M.Map String BaseType]

newtype Eval env err a = Eval { runEval :: StateT env (ExceptT err IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
                MonadIO, MonadError err, MonadState env)

primitiveOps :: M.Map (String, Int) Dynamic
primitiveOps = M.fromList [
    (("print", 1), toDyn (fmap (const Unit) . putStrLn)),
    (("readLn", 1), toDyn (fmap Str readLn)),
    (("+", 2), toDyn (\a b -> return @IO $ Numb (a + b)))
  ]

applyPrimitive :: (String, Dynamic) -> [PrimitiveType] -> IO (Either String PrimitiveType)
applyPrimitive (opName, op) [] = _
-- sequence $
   --                                maybeToRight ("Failed to apply primitive 0-operation" <> opName)
     ---                                           (fromDynamic op :: Maybe (IO PrimitiveType))

type LispEval = Eval Env String BaseType

evalM :: LispAst -> LispEval
evalM (Const v) = pure v
evalM (Var x) = do
  env <- get
  case lookupInEnv x env of
    Just v -> pure v
    Nothing -> throwError $ "Failed to lookup variable " <> x <> " in the current env"
evalM (Sf (If cond th els)) = do
    evaluatedCond <- evalM cond
    case evaluatedCond of
      Primitive (Bl b) -> evalM (if b then th else els)
      _ -> throwError "Provided condition is not of the type boolean"
evalM (Sf (Define vname body)) = do
    evBody <- evalM body
    env  <- get
    put (defineInEnv vname evBody env)
    return (Primitive Unit)
evalM (Sf (Assign vname body)) = do
    env <- get
    case lookupInEnv vname env of
      Just _ -> do
        evBody <- evalM body
        put (defineInEnv vname evBody env)
        return (Primitive Unit)
      _ -> throwError $ "Variable " <> vname <> " is not defined in the current environement"
evalM (Sf (DefineProc name bindings body)) = evalM (Sf (Define name (Const (Lambda bindings body))))
evalM (Sf (Begin procs)) = reduce (>>) (evalM <$> procs)
evalM (App operator operands) = do
    evOpt <- evalM operator
    args  <- sequence (evalM <$> operands)
    case evOpt of
      Primitive (Str optName) -> discardState $ applyM optName args
      _ -> throwError "Operator name has invalid type"

applyM :: String -> [BaseType] -> LispEval
applyM opName ops = _

lookupInEnv :: String -> Env -> Maybe BaseType
lookupInEnv x (Env []) = Nothing
lookupInEnv x (Env (e:es)) = case M.lookup x e of
                   Just s -> Just s
                   Nothing -> lookupInEnv x (Env es)

defineInEnv :: String -> BaseType -> Env -> Env
defineInEnv s v (Env (e:es)) = Env (M.insert s v e:es)

discardState f = do
  s <- get
  a <- f
  put s
  return a

reduce f l = foldl' f (NL.head l) (NL.tail l)
