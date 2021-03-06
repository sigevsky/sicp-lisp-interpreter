{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module LispEval where

import qualified Data.Map as M
import Data.Either.Combinators (maybeToRight)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl', toList)
import Control.Monad
import Control.Monad.Except
import Control.Applicative (Applicative, Alternative)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Reader (ReaderT (..), ask, local, asks, MonadReader, runReaderT)
import Data.IORef (IORef, readIORef, modifyIORef, newIORef)
import Control.Monad.Writer (MonadWriter)
import qualified Data.List.NonEmpty as NE
import Data.Dynamic
import PrimitiveOps (applyPrimitive, primitivesOps)
import Types

newtype EvalT env err a = EvalT { runEval :: ReaderT env (ExceptT err IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
                MonadIO, MonadError err, MonadReader env)

type LispEval a = EvalT Env EvalError a

evalM :: LispAst -> LispEval RtType
evalM (Const pt) = pure . primitiveToRuntimeType $ pt
evalM (Var x) = lookupInEnv x
evalM (If cond th els) = do
    evaluatedCond <- evalM cond
    case evaluatedCond of
      Bl b -> evalM (if b then th else els)
      _ -> throwError IncorrectCondType
evalM (Define vname body) = do
    evBody <- evalM body
    putInCurrentFrame vname evBody

evalM (Assign vname body) = do
    evBody <- evalM body
    updateInEnv vname evBody

evalM (Lambda bindings body) = asks (Proc . Closure bindings body)
evalM (Begin procs) = evalMSequence procs
evalM (App operator operands) = do
    evOpt <- evalM operator
    args  <- sequence (evalM <$> operands)
    case evOpt of
      Proc (Closure bindings body procEnv) | length bindings == length args -> do
          newFrame <- liftIO (newIORef (M.fromList $ zip bindings args))
          local (\_ -> appendFrame newFrame procEnv) $ evalMSequence body
      Proc (PrimProc opName bindNum) | bindNum == length args -> applyPrimOp opName args
      Proc (Closure bindings body _)  | length bindings /= length args -> throwError . ApError $ IncorrectNumOfArgs "lambda" (length args) (length bindings)
      Proc (PrimProc opName bindNum) | bindNum /= length args -> throwError . ApError $ IncorrectNumOfArgs opName (length args) bindNum
      _ -> throwError $ InvalidOperatorType evOpt
  where
    applyPrimOp opName ops = EvalT . lift $ withExceptT (ApError . PrimProcApplyErr) (applyPrimitive opName ops)

evalMSequence :: NE.NonEmpty LispAst -> LispEval RtType
evalMSequence procs = reduce (>>) (evalM <$> procs)

evaluate :: LispAst -> IO (Either EvalError RtType)
evaluate ast = initialEnv >>= \env -> runExceptT $ runReaderT (runEval . evalM $ ast) env

initialEnv :: IO Env
initialEnv = Env . (: []) <$> newIORef (M.fromList primitivesOps)

freezeEnv :: LispEval FixedEnv
freezeEnv = ask >>= \(Env e) -> liftIO (FixedEnv <$> traverse readIORef e)

lookupInEnv :: String -> LispEval RtType
lookupInEnv x = do
  env <- ask
  findVar env
  where
    findVar (Env []) =  freezeEnv >>= \fenv -> throwError (VarNotFound x fenv)
    findVar (Env (e:es)) = do
      frame <- liftIO $ readIORef e
      case M.lookup x frame of
        Just s -> pure s
        Nothing -> findVar (Env es)

updateInEnv :: String -> RtType -> LispEval RtType
updateInEnv key val = do
  env <- ask
  findAndReplace env
  where
    findAndReplace (Env []) =  freezeEnv >>= \fenv -> throwError (VarNotFound key fenv)
    findAndReplace (Env (e:es)) = do
      frame <- liftIO $ readIORef e
      case M.lookup key frame of
        Just s -> liftIO (modifyIORef e (M.insert key val)) >> pure Unit
        Nothing -> findAndReplace (Env es)

putInCurrentFrame :: String -> RtType -> LispEval RtType
putInCurrentFrame s v = ask >>= \case
  Env [] -> throwError EmptyEnvAccess
  Env (e:es) -> liftIO (modifyIORef e (M.insert s v)) >> pure Unit

appendFrame :: IORef Frame -> Env -> Env
appendFrame m (Env ms) = Env (m:ms)

reduce f l = foldl' f (NE.head l) (NE.tail l)
