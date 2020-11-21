module ExprExpander where

import Types
import qualified Data.List.NonEmpty as NE
import Data.Foldable (foldl', toList)

both :: (a -> b) -> (a, a) -> (b, b)
both f (q, w) = (f q, f w)

expandSyntax :: LispSyntax -> LispAst
expandSyntax (ConstS t) = Const (primitiveSyntaxToAst t)
expandSyntax (VarS s) = Var s
-- trivial
expandSyntax (AppS op args) = App (expandSyntax op) (expandSyntax <$> args)
expandSyntax (SfSyntax (DefineS s ls)) = Define s (expandSyntax ls)
expandSyntax (SfSyntax (AssignS s ls)) = Assign s (expandSyntax ls)
expandSyntax (SfSyntax (BeginS ls)) = Begin (expandSyntax <$> ls)
expandSyntax (SfSyntax (LambdaS bindings body)) = Lambda bindings (expandSyntax <$> body)
expandSyntax (SfSyntax (IfS c th els)) = If (expandSyntax c) (expandSyntax th) (expandSyntax els)
-- desugaring
expandSyntax (SfSyntax (CondS l mbElse)) =
    let
      expandedL = both expandSyntax <$> l
      (lastCond, lastThen) NE.:| lst = NE.reverse expandedL
      lastIf = If lastCond lastThen (maybe (Const UnitAst) expandSyntax mbElse)
    in foldl' (\els (cond, res) -> If cond res els) lastIf lst
expandSyntax (SfSyntax (DefineProcS name bindings body)) = Define name (Lambda bindings (expandSyntax <$> body))
expandSyntax (SfSyntax (LetS argPairs body)) = App (Lambda (toList $ fst <$> argPairs) (expandSyntax <$> body))
                                             (toList $ snd . fmap expandSyntax <$> argPairs)
expandSyntax (SfSyntax (LetAsteriskS argPairs body)) =
    let
      (lastBind, lastArg) NE.:| lst = NE.reverse (fmap (fmap expandSyntax) argPairs)
      lastL = App (Lambda [lastBind] (expandSyntax <$> body)) [lastArg]
      desugared = foldl' (\nextLambda (bind, arg) -> App (Lambda [bind] (nextLambda NE.:| [])) [arg]) lastL lst
    in desugared
expandSyntax (SfSyntax (OrS args)) = 
  let
      lastCond NE.:| lst = NE.reverse (expandSyntax <$> args)
  in foldl' (\els cd -> If cd (Const (BlAst True)) els) lastCond lst
expandSyntax (SfSyntax (AndS args)) = 
  let
      lastCond NE.:| lst = NE.reverse (expandSyntax <$> args)
  in foldl' (\els cd -> If cd els (Const (BlAst False))) lastCond lst
    
-- and [a, b, c] -> lambda a -> if (a) then a else b
