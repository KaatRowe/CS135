module Lisp where

import Parsing 

--------------------------------------
-- Datatypes
--------------------------------------

data Value = NumV Int | BoolV Bool deriving Show

data Expr = Plus Expr Expr
          | Mult Expr Expr
          | Num Int 
          | TrueE 
          | FalseE 
          | If Expr Expr Expr
          | Id Identifier
          | FnApp Identifier Expr
          deriving Show


type Identifier = String

data FnDef = FnDef {fnName :: String, fnParams :: Identifier, fnBody :: Expr}
--------------------------------------
-- Parser
--------------------------------------

pExpr :: Parser Expr 
pExpr = pPlus <|> pMult <|> pMinus <|> pLiteral <|> pIf

pLiteral :: Parser Expr 
pLiteral = Num <$> natural <|> pTrue <|> pFalse 

pTrue :: Parser Expr 
pTrue = do symbol "#t" 
           return TrueE 

pFalse :: Parser Expr 
pFalse = do symbol "#f" 
            return FalseE

pPlus :: Parser Expr
pPlus = pFunc "+" (Plus <$> pExpr <*> pExpr)

pMult :: Parser Expr
pMult = pFunc "*" (Mult <$> pExpr <*> pExpr)

pMinus :: Parser Expr 
pMinus = pFunc "-" (minus <$> pExpr <*> pExpr) 
   where minus e1 e2 = Plus e1 (Mult (Num (-1)) e2) 

pIf :: Parser Expr 
pIf = pFunc "if" (If <$> pExpr <*> pExpr <*> pExpr)

pFunc :: String -> Parser Expr -> Parser Expr 
pFunc s p = do symbol "(" 
               symbol s 
               exp <- p 
               symbol ")" 
               return exp

{- pCond :: Parser Expr -> Parser Expr
pCond = pFunc "cond" pCases 

pCases = do symbol "("
            cnd <- pExpr
            bdy <- pExpr
            symbol ")"  -}



parseString :: String -> Expr
parseString s = case runParser pExpr s of
    [(x,"")] -> x 
    _        -> error "*** not parsable"

--------------------------------------
-- Interpreter
--------------------------------------

binOp :: (Int -> Int -> Int) -> (Value -> Value -> Value) 
binOp op (NumV x) (NumV y) = NumV (op x y) 
binOp _ _ _ = error "*** not numeric args"

interp :: [FnDef] -> Expr -> Value
interp fns exp = case exp of 
        Num n  -> NumV n
        TrueE  -> BoolV True
        FalseE -> BoolV False
        Plus x y -> binOp (+) (interp fns x) (interp fns y) 
        Mult x y -> binOp (*) (interp fns x) (interp fns y)

        If cnd thn els -> case interp fns cnd of 
                            BoolV True  -> interp fns thn 
                            BoolV False -> interp fns els 
                            _           -> error "*** not a Bool"

        
        FnApp f x -> let (FnDef _ param body) = lookupFn f fns in interp fns (subst param x body)

        Id id -> error "*** unbounded identifier"

lookupFn :: Identifier -> [FnDef] -> FnDef
lookupFn = undefined

subst :: Identifier -> Expr -> Expr -> Expr 
subst param arg body = 
    let sub = subst param arg
    in case body of
    Plus x y -> Plus (sub x) (sub y)
    Mult x y -> Mult (sub x) (sub y)
    Num n -> Num n
    TrueE -> TrueE
    FalseE -> FalseE

    If cnd thn els -> If (sub cnd) (sub thn) (sub els)

    FnApp f x -> FnApp f (sub x)

    Id var -> if param == var then arg else Id var
