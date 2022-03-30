module Lisp where

import Parsing 

data Value = NumV Int | BoolV Bool 
    deriving Show
    

data Expr = 
      Plus Expr Expr 
    | Multi Expr Expr 
    | Num Int 
    | TrueE 
    | FalseE 
    | If Expr Expr Expr
    deriving Show


pExpr :: Parser Expr
pExpr = pPlus <|> pMult <|> pLiteral

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
    where minus e1 e2 = Plus e1 (Mult (Num (-1) e2))

pFunc :: String -> Parser Expr -> Parser Expr 
pFunc s p = do symbol "(" 
               symbol s 
               exp <- p 
               symbol ")" 
               return exp

pString :: String -> Expr
pString str = case runParser pExpr str of
    [(x, "")] -> x
    _ -> error "*** Not Parsable"


----------------------------------------------------------------

binOp :: (Int -> Int -> Int) -> (Value -> Value -> Value)
binOp op (NumV x) (NumV y) = NumV (op x y)
binOp _ _ _ = error "*** Not numeric args"

interp :: Expr -> Value
interp exp = case exp of 
    Num n -> NumV n
    TrueE -> BoolV True
    FalseE -> BoolV False
    Plus x y -> binOp (+) (interp x) (interp y)
    Multi x y -> binOp (*) (interp x) (interp y)

    If cnd thn els -> case interp cnd of 
        BoolV True -> interp thn
        BoolV False -> interp els
        _ -> error "*** Not a Bool"
        