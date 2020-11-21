{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispEval where

import qualified Data.Map as M
import Data.Either.Combinators (maybeToRight)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl', toList)
import Control.Monad
import Control.Monad.Except
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Cont (MonadCont)
import Control.Monad.State (MonadState, StateT (..), get, put, gets)
import Control.Monad.Writer (MonadWriter)
import qualified Data.List.NonEmpty as NE
import Data.Dynamic
import PrimitiveOps (applyPrimitive, primitivesOps)
import Types

newtype EvalT env err a = EvalT { runEval :: StateT env (ExceptT err IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
                MonadIO, MonadError err, MonadState env)

type LispEval = EvalT Env EvalError RtType

evalM :: LispAst -> LispEval
evalM (Const pt) = pure . primitiveToRuntimeType $ pt
evalM (Var x) = do
  env <- get
  case lookupInEnv x env of
    Just v -> pure v
    Nothing -> throwError (VarNotFound x env)
evalM (If cond th els) = do
    evaluatedCond <- evalM cond
    case evaluatedCond of
      Bl b -> evalM (if b then th else els)
      _ -> throwError IncorrectCondType
evalM (Define vname body) = do
    evBody <- evalM body
    env  <- get
    put (defineInEnv vname evBody env)
    return Unit
evalM (Assign vname body) = do
    env <- get
    case lookupInEnv vname env of
      Just _ -> do
        evBody <- evalM body
        put (defineInEnv vname evBody env)
        return Unit
      _ -> throwError $ VarNotFound vname env
evalM (Lambda bindings body) = gets (Proc . Closure bindings body)
evalM (Begin procs) = evalMSequence procs
evalM (App operator operands) = do
    evOpt <- evalM operator
    args  <- sequence (evalM <$> operands)
    case evOpt of
      Proc (Closure bindings body procEnv) | length bindings == length args -> discardState $ do
          let newFrame = M.fromList $ zip bindings args
          put $ appendFrame newFrame procEnv
          evalMSequence body
      Proc (PrimProc opName bindNum) | bindNum == length args -> applyPrimOp opName args
      Proc (Closure bindings body procEnv)  | length bindings /= length args -> throwError . ApError $ IncorrectNumOfArgs "lambda" (length args) (length bindings)
      Proc (PrimProc opName bindNum) | bindNum /= length args -> throwError . ApError $ IncorrectNumOfArgs opName (length args) bindNum
      _ -> throwError $ InvalidOperatorType evOpt
  where
    applyPrimOp opName ops = EvalT . lift $ withExceptT (ApError . PrimProcApplyErr) (applyPrimitive opName ops)

liftError f (EvalT (StateT st)) = EvalT (StateT (withExceptT f . st))

evalMSequence :: NE.NonEmpty LispAst -> LispEval
evalMSequence procs = reduce (>>) (evalM <$> procs)

evaluate :: LispAst -> IO (Either EvalError RtType)
evaluate ast = runExceptT . fmap fst $ runStateT (runEval . evalM $ ast) initialEnv

initialEnv :: Env
initialEnv = Env [M.fromList primitivesOps]

lookupInEnv :: String -> Env -> Maybe RtType
lookupInEnv x (Env []) = Nothing
lookupInEnv x (Env (e:es)) = case M.lookup x e of
                   Just s -> Just s
                   Nothing -> lookupInEnv x (Env es)

defineInEnv :: String -> RtType -> Env -> Env
defineInEnv s v (Env (e:es)) = Env (M.insert s v e:es)

appendFrame :: M.Map String RtType -> Env -> Env
appendFrame m (Env ms) = Env (m:ms)

discardState f = do
  s <- get
  a <- f
  put s
  return a

reduce f l = foldl' f (NE.head l) (NE.tail l)
