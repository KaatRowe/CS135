module Arith where


import Parsing -- Case will give us this class

data Expr = Plus Expr Expr | Multi Expr Expr | Num Int
    deriving Show


pExpr :: Parser Expr
pExpr = do t <- pTerm
           symbol "+"
           Plus t <$> pExpr
        <|> pTerm

pTerm :: Parser Expr
pTerm = do f <- pFactor
           symbol "*"
           Multi f <$> pTerm
        <|> pFactor

pFactor :: Parser Expr
pFactor = do
            symbol "("
            exp <- pExpr
            symbol ")"
            return exp
        <|> Num <$> natural


parseString :: String -> Expr
parseString str = case runParser pExpr str of
    [(x, "")] -> x
    _ -> error "*** not Parsable expression"