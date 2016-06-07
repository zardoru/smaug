module Smaug.SemanticRules.SymbolsAndScope where
import Smaug.LangADT
import Data.List (group, sort, nub, intercalate, foldl')
import Control.Monad (forM, foldM)
import Data.Either

type Scope = Int
type Symbol = (Int, String)
type SymList = [Symbol] 
type ErrorSeq = Either SemanticRuleStatus SymList

-- Symbol Table
-- generalize into foldable prob.
smaugCreateSymbolTable' :: Scope -> BodyExpr -> SymList
smaugCreateSymbolTable' scope expr =
    case expr of
        MainBody body -> concat $ fromStmtList body
        IfExpr expr body ebody -> (concat $ fromStmtList body) ++ (concat $ fromStmtList ebody) 
        WhileExpr expr body -> concat $ fromStmtList body
        LetExpr id expr -> [(scope, id)]
        AssnExpr id expr -> []
        BodyCallExpr expr -> []
        BreakStmt -> []
        ContinueStmt -> []
        ReturnStmt -> []
        ForExpr id x ds st bd -> (scope, id) : (concat $ fromStmtList bd)
        _ -> error $ "Unimplemented statement for symbol extraction (" ++ show expr ++ ")"
        where 
            nextScope = scope + 1
            fromStmtList = map (smaugCreateSymbolTable' nextScope)

getDuplicateSymbolsMessage :: SymList -> String
getDuplicateSymbolsMessage table =
    intercalate "," duplicateTable
    where duplicateTable = map snd . map head . filter (\x -> length x > 1) . group . sort $ table

noDuplicateSymbols :: BodyExpr -> SemanticRuleStatus
noDuplicateSymbols expr =
    case nub table == table of
        True -> Ok
        False -> 
            let 
                msg = "There are duplicate symbols at a scope level: " 
                        ++ getDuplicateSymbolsMessage table
            in SemanticError $ msg
    where table = smaugCreateSymbolTable expr

getScopeError i s = "Variable \"" ++ i ++ 
            "\" is not declared (yet?)" ++ " in the declaration at scope " ++ show s 

isInScope :: String -> SymList -> Scope -> SemanticRuleStatus
isInScope i t s = 
    case scopeCheckSuccesful of
        True -> Ok
        False -> SemanticError $ getScopeError i s 
    -- is in the scope level or below and this id is the table
    where 
        symb = filter (\x -> fst x <= s && snd x == i) t
        scopeCheckSuccesful = length symb > 0

hasValidReferences :: SymList -> Scope -> Expr -> SemanticRuleStatus 
hasValidReferences t s (Lit _) = Ok
hasValidReferences t s (QuoteStr _) = Ok
hasValidReferences t s (NullExpr) = Ok
hasValidReferences t s (Ident identifier) = isInScope identifier t s  
hasValidReferences t s (FunCallExpr id xps) = foldl' (&^) Ok $ map (hasValidReferences t s) xps
hasValidReferences t s x = hasValidReferences t s (getExprLeft x) 
                            &^ hasValidReferences t s (getExprRight x)

-- Si dados los símbolos iniciales t
-- en el ámbito s 
-- revisar acomulativamente si las expresiónes
-- poseen reglas de referencias válidas.
allExprsHaveValidReferences :: SymList -> Scope -> [BodyExpr] -> ErrorSeq 
allExprsHaveValidReferences t s e = foldM foldFunc t e
    where foldFunc acc x = validReferencesWalk s x acc

-- Revisa si la expresión en la declaración de la variable ident
-- dados los simbolos t en el ámbito scope
-- posee referencias válidas.        
checkLet :: String -> SymList -> Scope -> Expr -> ErrorSeq 
checkLet ident t scope expr =
    case ref of 
        Ok -> Right ((scope, ident) : t) 
        (SemanticError _) -> Left ref
    where 
        ref = hasValidReferences t scope expr

checkFor :: BodyExpr -> SymList -> Scope -> ErrorSeq
checkFor (ForExpr id xp dst st lexpr) syms scope =
    do
        newsym <- checkLet id syms (scope+1) xp
        allExprsHaveValidReferences newsym (scope+1) lexpr
        return syms

-- Genera un ErrorSeq a partir de una lista de símbolos
-- o un error (Equivalente a return con un tipo correcto)
wrap :: SymList -> SemanticRuleStatus -> ErrorSeq
wrap l s = 
    case s of
        Ok -> Right l
        s -> Left s

-- Dado un ámbito scope, el cuerpo symexpr y los símbolos syms
-- revisar si las referencias a variables son correctas.

validReferencesWalk :: Scope -> BodyExpr -> SymList -> ErrorSeq
validReferencesWalk scope symexpr syms = 
    case symexpr of 
        -- Generate symbol table only on the main body.
        (MainBody lexpr) -> allExprsHaveValidReferences syms nextScope lexpr
        -- And don't when we're walking into the tree.
        (BodyCallExpr expr) -> wrap syms $ hasValidReferences syms scope expr
        
        -- Usamos la lista de símbolos local (syms) para
        -- que cada bloque sea su propio ámbito en vez de
        -- hacer que cada nivel del árbol sea un ambito global a ese nivel.
        (IfExpr expr lbexpr elbexpr) -> do
            allExprsHaveValidReferences syms nextScope lbexpr
            allExprsHaveValidReferences syms nextScope elbexpr 
            wrap syms $ hasValidReferences syms scope expr
             
        (WhileExpr expr lbexpr) -> do
            allExprsHaveValidReferences syms nextScope lbexpr
            wrap syms $ hasValidReferences syms scope expr

        (ForExpr id xp dst st bd) -> checkFor symexpr syms nextScope 

        (AssnExpr id expr) -> wrap syms $ 
            isInScope id syms scope &^ hasValidReferences syms scope expr
        (LetExpr id expr) -> checkLet id syms scope expr
        _ -> error $ "Unimplemented statement for reference checking (" ++ show symexpr ++ ")"
    where
        nextScope = scope + 1 
    
-- Wrapper de estado inicial para validReferencesWalk.
validReferences expr = validReferencesWalk 0 expr []

-- Revisión de las reglas semánticas de símbolos.
symbolTableRules expr = 
    case noDuplicateSymbols expr of
        Ok -> hasValidReferences expr
        x -> x
        where 
            hasValidReferences xpr =
                case validReferences expr of
                    (Left err) -> err
                    (Right _) -> Ok  
    
-- Creación de tabla de símbolos en base a un statement.
smaugCreateSymbolTable :: BodyExpr -> SymList
smaugCreateSymbolTable = smaugCreateSymbolTable' 0