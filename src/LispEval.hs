{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module LispEval where

import qualified Data.Map as M
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (foldl', toList)
import Control.Monad
import Control.Monad.Except
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Cont (MonadCont)
import Control.Monad.State (MonadState, StateT (..), get, put)
import Control.Monad.Writer (MonadWriter)
import qualified Data.List.NonEmpty as NL
import Data.Dynamic
import PrimitiveOps (applyPrimitive)
import Types

newtype EvalT env err a = EvalT { runEval :: StateT env (ExceptT err IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
                MonadIO, MonadError err, MonadState env)

type LispEval = EvalT Env EvalError PrimitiveType

evalM :: LispAst -> LispEval
evalM (Const v) = pure v
evalM (Var x) = do
  env <- get
  case lookupInEnv x env of
    Just v -> pure v
    Nothing -> throwError (VarNotFound x env)
evalM (Sf (If cond th els)) = do
    evaluatedCond <- evalM cond
    case evaluatedCond of
      Bl b -> evalM (if b then th else els)
      _ -> throwError IncorrectCondType
evalM (Sf (Define vname body)) = do
    evBody <- evalM body
    env  <- get
    put (defineInEnv vname evBody env)
    return Unit
evalM (Sf (Assign vname body)) = do
    env <- get
    case lookupInEnv vname env of
      Just _ -> do
        evBody <- evalM body
        put (defineInEnv vname evBody env)
        return Unit
      _ -> throwError $ VarNotFound vname env
      
evalM (Sf (DefineProc name bindings body)) = evalM (Sf (Define name (Const (Lambda bindings body))))
evalM (Sf (Begin procs)) = reduce (>>) (evalM <$> procs)
evalM (Sf (Let argPairs body)) = evalM (App (Const (Lambda (toList $ fst <$> argPairs) body)) (toList $ snd <$> argPairs))
evalM (App operator operands) = do
    evOpt <- evalM operator
    args  <- sequence (evalM <$> operands)
    case evOpt of
      (Str optName) -> discardState $ applyM optName args
      _ -> throwError $ InvalidOperatorType evOpt

applyM :: String -> [PrimitiveType] -> LispEval
applyM opName ops = catchError applyPrimOp handler
  where
    applyPrimOp = EvalT . lift $ withExceptT (ApError . PrimProcApplyErr) (applyPrimitive opName ops)
    handler = \case
      (ApError (PrimProcApplyErr (ProcedureNotFound v))) -> do
        env <- get
        case lookupInEnv opName env of
          Just op -> case op of
            (Lambda bindings body) | length bindings == length ops -> do
              env <- get
              let newFrame = M.fromList $ zip bindings ops
              put $ appendFrame newFrame env
              evalM body
            (Lambda bindings _) | length bindings /= length ops -> throwError . ApError $ IncorrectNumOfArgs opName (length ops) (length bindings)
            _ -> throwError . ApError . ProcApplyError $ opName
          Nothing -> throwError . ApError $ NoDefProcedure opName env
      v -> throwError v

liftError f (EvalT (StateT st)) = EvalT (StateT (withExceptT f . st))

lookupInEnv :: String -> Env -> Maybe PrimitiveType
lookupInEnv x (Env []) = Nothing
lookupInEnv x (Env (e:es)) = case M.lookup x e of
                   Just s -> Just s
                   Nothing -> lookupInEnv x (Env es)

defineInEnv :: String -> PrimitiveType -> Env -> Env
defineInEnv s v (Env (e:es)) = Env (M.insert s v e:es)

appendFrame :: M.Map String PrimitiveType -> Env -> Env
appendFrame m (Env ms) = Env (m:ms)

discardState f = do
  s <- get
  a <- f
  put s
  return a

reduce f l = foldl' f (NL.head l) (NL.tail l)
