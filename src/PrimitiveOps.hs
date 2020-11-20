{-# LANGUAGE TypeApplications #-}
module PrimitiveOps (applyPrimitive, primitivesOps) where

import Data.Dynamic
import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as M
import Data.List.Index (ifoldl')
import Types (RtType(..), PrimProcApplyError(..), RtProc(..))
import Control.Monad.Except

dynamicMappings :: M.Map (String, Int) Dynamic
dynamicMappings = M.fromList [
    (("print", 1), toDyn printP),
    (("readLn", 0), toDyn readLnP),
    (("+", 2), toDyn additionP),
    (("*", 2), toDyn multP),
    (("abs", 1), toDyn absP)
  ]

primitivesOps :: [(String, RtType)]
primitivesOps = fmap (\key -> (fst key, Proc . uncurry PrimProc $ key)) keys
  where keys = fmap fst . M.toList $ dynamicMappings

additionP (Numb a) (Numb b) = Just . return @IO . Numb $ (a + b)
additionP _ _ = Nothing

absP :: RtType -> Maybe (IO RtType)
asbP (Numb a) = Just . return @IO . Numb $ abs a
absP _ = Nothing

multP (Numb a) (Numb b) = Just . return @IO . Numb $ (a * b)
multP _ _ = Nothing

readLnP = Just $ Str <$> readLn @String

printP = Just . fmap (const Unit) . putStrLn . (">> " <>) . showPrim

showPrim (Str s) = s
showPrim (Numb s) = show s
showPrim (Bl s) = if s then "true" else "false"
showPrim (Proc p) = show p
showPrim Unit = "()"

applyPrimitive :: String -> [RtType] -> ExceptT PrimProcApplyError IO RtType
applyPrimitive opName args = case mbOp of
  Nothing -> throwError $ ProcedureNotFound opName
  Just op -> ExceptT . sequence $ do
        resDyn <- ifoldl' (\eiF i val -> eiF >>= \f -> maybeToRight (FailedToApplyArg i opName) $ dynApply f (toDyn val))
                          (Right op)
                          args
        resT   <- maybeToRight (InvalidProcedure opName) (fromDynamic resDyn :: Maybe (Maybe (IO RtType)))
        maybeToRight (InvalidTypesPassed opName) resT
  where mbOp = M.lookup (opName, length args) dynamicMappings
