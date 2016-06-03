module Smaug.LangADT where
    
data Expr =
    Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Leq Expr Expr
    | Geq Expr Expr
    | Neq Expr Expr
    | Eq Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Ident String
    | Lit Double
    | QuoteStr String 
    | FunCallExpr String [Expr]
    | NullExpr
    deriving Show
    
    
data BodyExpr = 
    LetExpr String Expr
    | AssnExpr String Expr
    | WhileExpr Expr [BodyExpr]
    | IfExpr Expr [BodyExpr]
    | ForExpr String Double Double Double [BodyExpr]
    | BodyCallExpr Expr
    | MainBody [BodyExpr]
    deriving Show
    
data SemanticRuleStatus = 
    SemanticError String | Ok
    deriving Show
    
getExprLeft :: Expr -> Expr
getExprLeft (Lt l r) = l
getExprLeft (Gt l r) = l
getExprLeft (Leq l r) = l
getExprLeft (Geq l r) = l
getExprLeft (Eq l r) = l
getExprLeft (Neq l r) = l
getExprLeft (Add l r) = l
getExprLeft (Mul l r) = l
getExprLeft (Sub l r) = l
getExprLeft (Div l r) = l 
getExprLeft (And l r) = l
getExprLeft (Or l r) = l
getExprLeft _ = NullExpr

getExprRight :: Expr -> Expr
getExprRight (Lt l r) = r
getExprRight (Gt l r) = r
getExprRight (Leq l r) = r
getExprRight (Geq l r) = r
getExprRight (Eq l r) = r
getExprRight (Neq l r) = r
getExprRight (Add l r) = r
getExprRight (Mul l r) = r
getExprRight (Sub l r) = r
getExprRight (Div l r) = r 
getExprRight (And l r) = r
getExprRight (Or l r) = r
getExprRight _ = NullExpr

(&^) :: SemanticRuleStatus -> SemanticRuleStatus -> SemanticRuleStatus
a &^ b = 
    case a of
        Ok -> b
        SemanticError x -> a 