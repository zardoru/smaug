module Smaug.TreeParser where
import Smaug.Parser
import Data.List (foldl')

{-
    Mis reglas sintácticas respecto al ámbito:
        * let requiere una inicialización - punto.
        * una declaración de variable no puede 
        tener símbolos repetidos en el ámbito local.
        * Se usa el ámbito local primero, y de ahí hacia afuera.
        * se hace el espacio para las variables del let al inicio de la función
        
        * alt sencillo: todas las variables son globales
-}

data SemanticRuleStatus = 
    SemanticError String | Ok
    deriving Show

-- No inequality chaining
checkIsNotIneq :: Expr -> SemanticRuleStatus
checkIsNotIneq (Lt _ _) = SemanticError "< cannot be chained"
checkIsNotIneq (Gt _ _) = SemanticError "> cannot be chained" 
checkIsNotIneq (Geq _ _) = SemanticError ">= cannot be chained" 
checkIsNotIneq (Leq _ _) = SemanticError "<= cannot be chained"
checkIsNotIneq (Eq _ _) = SemanticError "== cannot be chained"
checkIsNotIneq (Neq _ _) = SemanticError "!= cannot be chained"
checkIsNotIneq _ = Ok  

checkIneqExpr :: Expr -> Expr -> SemanticRuleStatus
checkIneqExpr l r = checkIsNotIneq l &^ checkIsNotIneq r

-- No strings on either side
-- cambiar esto a revisar que sea la iz. y de. del mismo tipo
checkIsNoStr :: Expr -> SemanticRuleStatus
checkIsNoStr (QuoteStr expr) = SemanticError "A string is not a valid type for this operation."
checkIsNoStr (Lit x) = Ok
-- recursive call
checkIsNoStr xpr = checkExpr xpr

checkNoStr :: Expr -> Expr -> SemanticRuleStatus
checkNoStr l r = checkIsNoStr l &^ checkIsNoStr r

checkExpr :: Expr -> SemanticRuleStatus
checkExpr (Lt l r) = checkIneqExpr l r
checkExpr (Gt l r) = checkIneqExpr l r
checkExpr (Leq l r) = checkIneqExpr l r
checkExpr (Geq l r) = checkIneqExpr l r
checkExpr (Eq l r) = checkIneqExpr l r
checkExpr (Neq l r) = checkIneqExpr l r
checkExpr (Add l r) = checkNoStr l r 
checkExpr (Mul l r) = checkNoStr l r
checkExpr (Sub l r) = checkNoStr l r
checkExpr (Div l r) = checkNoStr l r
checkExpr (FunCallExpr id xps) = checkRulesForAllExprs xps 
checkExpr _ = Ok

(&^) :: SemanticRuleStatus -> SemanticRuleStatus -> SemanticRuleStatus
a &^ b = 
    case a of
        Ok -> b
        SemanticError x -> a 

checkRulesInList :: [BodyExpr] -> SemanticRuleStatus
checkRulesInList xprl = foldl' reduceFn startVal xprl
    where reduceFn acc v = acc &^ (smaugCheckSemanticRules v)
          startVal = Ok
          
checkRulesForAllExprs :: [Expr] -> SemanticRuleStatus
checkRulesForAllExprs (x:xpr) = checkExpr x &^ checkRulesForAllExprs xpr
checkRulesForAllExprs [] = Ok

-- Symbol Table
-- generalize into foldable prob.
smaugCreateSymbolTable' :: Int -> BodyExpr -> [(Int, String)]
smaugCreateSymbolTable' scope expr =
    case expr of
        MainBody body -> concat $ fromStmtList body
        IfExpr expr body -> concat $ fromStmtList body
        WhileExpr expr body -> concat $ fromStmtList body
        LetExpr id expr -> [(scope, id)]
        AssnExpr id expr -> []
        BodyCallExpr expr -> []  
        _ -> error "Unimplemented expression for symbol extraction"
        where 
            nextScope = scope + 1
            fromStmtList = map (smaugCreateSymbolTable' nextScope)

smaugCreateSymbolTable :: BodyExpr -> [(Int, String)]
smaugCreateSymbolTable = smaugCreateSymbolTable' 0

-- Check semantic rules
smaugCheckSemanticRules :: BodyExpr -> SemanticRuleStatus
smaugCheckSemanticRules expr =
    case expr of
        MainBody body -> checkRulesInList body
        IfExpr expr body -> checkExpr expr &^ checkRulesInList body
        WhileExpr expr body -> checkExpr expr &^ checkRulesInList body
        AssnExpr id expr -> checkExpr expr
        LetExpr id expr -> checkExpr expr
        BodyCallExpr expr -> checkExpr expr  
        _ -> error "Unimplemented semantic check"