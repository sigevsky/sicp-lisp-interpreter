module ExprExpander where

import Types
import qualified Data.List.NonEmpty as NE
import Data.Foldable (foldl', toList)

both :: (a -> b) -> (a, a) -> (b, b)
both f (q, w) = (f q, f w)

expand :: LispSyntax -> LispAst
expand (ConstS t) = Const (primitiveSyntaxToAst t)
expand (VarS s) = Var s
-- trivial
expand (AppS op args) = App (expand op) (expand <$> args)
expand (SfSyntax (DefineS s ls)) = Define s (expand ls)
expand (SfSyntax (AssignS s ls)) = Assign s (expand ls)
expand (SfSyntax (BeginS ls)) = Begin (expand <$> ls)
expand (SfSyntax (LambdaS bindings body)) = Lambda bindings (expand <$> body)
expand (SfSyntax (IfS c th els)) = If (expand c) (expand th) (expand els)
-- desugaring
expand (SfSyntax (CondS l mbElse)) =
    let
      expandedL = both expand <$> l
      (lastCond, lastThen) NE.:| lst = NE.reverse expandedL
      lastIf = If lastCond lastThen (maybe (Const UnitAst) expand mbElse)
    in foldl' (\els (cond, res) -> If cond res els) lastIf lst
expand (SfSyntax (DefineProcS name bindings body)) = Define name (Lambda bindings (expand <$> body))
expand (SfSyntax (LetS argPairs body)) = App (Lambda (toList $ fst <$> argPairs) (expand <$> body))
                                             (toList $ snd . fmap expand <$> argPairs)
expand (SfSyntax (LetAsteriskS argPairs body)) =
    let
      (lastBind, lastArg) NE.:| lst = NE.reverse (fmap (fmap expand) argPairs)
      lastL = App (Lambda [lastBind] (expand <$> body)) [lastArg]
      desugared = foldl' (\nextLambda (bind, arg) -> App (Lambda [bind] (nextLambda NE.:| [])) [arg]) lastL lst
    in desugared

