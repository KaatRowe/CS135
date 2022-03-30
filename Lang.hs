module Lang where

import Parsing

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data Value = NumV Int
           | BoolV Bool
           deriving (Show,Eq)

data Expr = NumE Int
          | TrueE
          | FalseE
          | Op Operator Expr Expr
          | If Expr Expr Expr
          deriving (Show, Eq)

data Operator = Plus
              | Mult
              | Equal
              | LessThan
              deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = pPlus <|> pMult <|> pEqual <|> pLessThan <|> pIf <|> pSugar <|> pLiteral
  where pPlus = pFunc "+"  $ Op Plus <$> pExpr <*> pExpr
        pMult = pFunc "*"  $ Op Mult <$> pExpr <*> pExpr
        pEqual = pFunc "=" $ Op Equal <$> pExpr <*> pExpr
        pLessThan = pFunc "<" $ Op LessThan <$> pExpr <*> pExpr
        pIf   = pFunc "if" $ If      <$> pExpr <*> pExpr <*> pExpr

pSugar :: Parser Expr
pSugar = pMinus <|> pAnd <|> pOr <|> pLessThanEqualTo <|> pGreaterThan <|> pGreaterThanEqualTo <|> pNot
  where pMinus = pFunc "-"   $ minusD <$> pExpr <*> pExpr
        pAnd   = pFunc "and" $ andD   <$> pExpr <*> pExpr
        pOr    = pFunc "or"  $ orD    <$> pExpr <*> pExpr
        pLessThanEqualTo = pFunc "<=" $ lessThanEqualToD <$> pExpr <*> pExpr
        pGreaterThan = pFunc ">" $ greaterThanD <$> pExpr <*> pExpr
        pGreaterThanEqualTo = pFunc ">=" $ greaterThanEqualToD <$> pExpr <*> pExpr
        pNot   = pFunc "not" $ notD   <$> pExpr
        minusD l r = Op Plus l (Op Mult r (NumE (-1)))
        andD l r   = If l r FalseE
        orD l      = If l TrueE
        lessThanEqualToD l r = Op LessThan l (Op Equal l r)
        greaterThanD l r = Op LessThan r l
        greaterThanEqualToD l r = Op LessThan r (Op Equal r l)
        notD x     = If x FalseE TrueE

pLiteral :: Parser Expr
pLiteral = pNum <|> pTrue <|> pFalse
  where pNum   = NumE <$> natural
        pTrue  = symbol "#t" >> return TrueE
        pFalse = symbol "#f" >> return FalseE

pFunc :: String -> Parser a -> Parser a
pFunc sym p = do symbol "("
                 symbol sym
                 exp <- p
                 symbol ")"
                 return exp

parseString :: String -> Expr
parseString s = case runParser pExpr s of
  [(x,"")] -> x
  _        -> error "*** not parsable"

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

interp :: Expr -> Value
interp exp = case exp of
  NumE n -> NumV n
  TrueE -> BoolV True
  FalseE -> BoolV False
  Op Plus x y -> opTable Plus (interp x) (interp y)
  Op Mult x y -> opTable Mult (interp x) (interp y)
  Op Equal x y -> opTable Equal (interp x) (interp y) 
  Op LessThan x y -> opTable LessThan (interp x) (interp y)

  If cnd thn els -> case interp cnd of
    BoolV True -> interp thn
    BoolV False -> interp els
    _ -> error "*** not a Bool"



opTable :: Operator -> (Value -> Value -> Value)
opTable Plus = \(NumV x) (NumV y) -> NumV (x + y)
opTable Mult = \(NumV x) (NumV y) -> NumV (x * y)
opTable Equal = \(NumV x) (NumV y) -> if x == y then BoolV True else BoolV False
opTable LessThan = \(NumV x) (NumV y) -> if x < y then BoolV True else BoolV False

