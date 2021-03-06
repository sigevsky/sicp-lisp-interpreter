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
    (("display", 1), toDyn printP),
    (("readLn", 0), toDyn readLnP),
    (("+", 2), toDyn additionP),
    (("-", 2), toDyn substractP),
    (("*", 2), toDyn multP),
    (("abs", 1), toDyn absP),
    ((">", 2), toDyn gtP),
    (("<", 2), toDyn ltP),
    (("=", 2), toDyn eqNumP),
    ((">=", 2), toDyn gteP),
    (("eq?", 2), toDyn eqP),
    (("equal?", 2), toDyn eqP),
    (("not", 1), toDyn notP)
  ]

primitivesOps :: [(String, RtType)]
primitivesOps = fmap (\key -> (fst key, Proc . uncurry PrimProc $ key)) keys
  where keys = fmap fst . M.toList $ dynamicMappings

gteP (Numb a) (Numb b) = Just . return @IO . Bl $ (a >= b)
gteP _ _ = Nothing


notP (Bl b) = Just . return @IO . Bl $ not b
notP _ = Nothing

additionP (Numb a) (Numb b) = Just . return @IO . Numb $ (a + b)
additionP _ _ = Nothing

substractP (Numb a) (Numb b) = Just . return @IO . Numb $ (a - b)
substractP _ _ = Nothing

absP :: RtType -> Maybe (IO RtType)
asbP (Numb a) = Just . return @IO . Numb $ abs a
absP _ = Nothing

eqP :: RtType -> RtType -> Maybe (IO RtType)
eqP a b = Just . return @IO . Bl $ (a == b)

ltP (Numb a) (Numb b) = Just . return @IO . Bl $ (a < b)
ltP _ _ = Nothing

gtP (Numb a) (Numb b) = Just . return @IO . Bl $ (a > b)
gtP _ _ = Nothing

eqNumP (Numb a) (Numb b) = Just . return @IO . Bl $ (a == b)
eqNumP _ _ = Nothing

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
