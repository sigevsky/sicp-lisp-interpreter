{-# LANGUAGE TypeApplications #-}
module PrimitiveOps (applyPrimitive) where

import Data.Dynamic
import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as M
import Data.List.Index (ifoldl')
import Types (PrimitiveType(..), PrimProcApplyError(..))
import Control.Monad.Except

primitiveOps :: M.Map (String, Int) Dynamic
primitiveOps = M.fromList [
    (("print", 1), toDyn printP),
    (("readLn", 0), toDyn readLnP),
    (("+", 2), toDyn additionP)
  ]

additionP (Numb a) (Numb b) = Just . return @IO . Numb $ (a + b)
additionP _ _ = Nothing

readLnP = Just $ Str <$> readLn @String

printP = Just . fmap (const Unit) . putStrLn . showPrim

showPrim (Str s) = s
showPrim (Numb s) = show s
showPrim (Bl s) = if s then "true" else "false"
showPrim (Lambda _ _) = "<lambda>"
showPrim Unit = "()"

applyPrimitive :: String -> [PrimitiveType] -> ExceptT PrimProcApplyError IO PrimitiveType
applyPrimitive opName args = case mbOp of
  Nothing -> throwError $ ProcedureNotFound opName
  Just op -> ExceptT . sequence $ do
        resDyn <- ifoldl' (\eiF i val -> eiF >>= \f -> maybeToRight (FailedToApplyArg i opName) $ dynApply f (toDyn val))
                          (Right op)
                          args
        resT   <- maybeToRight (InvalidProcedure opName) (fromDynamic resDyn :: Maybe (Maybe (IO PrimitiveType)))
        maybeToRight (InvalidTypesPassed opName) resT
  where mbOp = M.lookup (opName, length args) primitiveOps
