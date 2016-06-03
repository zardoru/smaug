module Smaug.MathEval where
import Smaug.LangADT
import Data.Maybe
import Data.Either

-- Optimización simple (reducción de expresiones. Nothing => no optimizable.)
mathEval :: Expr -> Maybe Double
mathEval (Add l r) = (+) <$> mathEval l <*> mathEval r
mathEval (Mul l r) = (*) <$> mathEval l <*> mathEval r
mathEval (Sub l r) = (-) <$> mathEval l <*> mathEval r
mathEval (Div l r) = (/) <$> mathEval l <*> mathEval r
mathEval (Lit x) = Just $ x
mathEval _ = Nothing

mathEvalEither :: Either s Expr -> Either s (Maybe Double)
mathEvalEither xpr = mathEval <$> xpr