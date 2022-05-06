module Lang where

import Parsing

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type Identifier = String

data Value = NumV Int
           | BoolV Bool
           | FnV [Identifier] Expr Env -- Parameters, Body, Closure
           deriving (Show,Eq)

type Env = [(Identifier,Value)]

data Expr = NumE Int
          | TrueE
          | FalseE
          | Op Operator Expr Expr
          | If Expr Expr Expr
          | FnDef [Identifier] Expr -- Parameters, Body
          | FnApp Expr [Expr]       -- Function expression
          | Id Identifier
          deriving (Show, Eq)

data Operator = Plus
              | Mult
              | Equal
              | LessThan
              deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
 
parens :: Parser a -> Parser a
parens p = do symbol "("
              exp <- p
              symbol ")"
              return exp

pExpr :: Parser Expr
pExpr = parens (pBuiltIn <|> pFnApp) <|> pLiteral <|> pId
  where pFnApp   = FnApp <$> pExpr <*> many pExpr
        pLiteral = pNum <|> pTrue <|> pFalse
        pNum     = NumE <$> natural
        pTrue    = symbol "#t" >> return TrueE
        pFalse   = symbol "#f" >> return FalseE
        pId      = Id <$> identifier

pBuiltIn :: Parser Expr
pBuiltIn =
  do sym <- identifier
     case sym of
       "if"     -> If <$> pExpr <*> pExpr <*> pExpr
       "lambda" -> FnDef <$> parens (many identifier) <*> pExpr
       "not"    -> If <$> pExpr <*> return FalseE <*> return TrueE
       "cond"   -> pCondCases
       "let"    -> do 
                      letCases
       _        -> if sym `elem` binarySymbols
                   then binaryParseTable sym <$> pExpr <*> pExpr
                   else failure


letCases :: Parser Expr
letCases = vars <|> body
  vars = do symbol "("
          id <- identifier
          exp <- pExpr
          symbol ")"

  body = 


pCondCases :: Parser Expr
pCondCases =
  do symbol "("
     cnd <- pExpr
     thn <- pExpr
     symbol ")"
     if cnd == Id "else"
     then return thn
     else If cnd thn <$> pCondCases

binarySymbols :: [String]
binarySymbols = ["+", "*", "=", "<", "-", "and", "or", "<=", ">=", ">"]

binaryParseTable :: String -> Expr -> Expr -> Expr
binaryParseTable sym l r = case sym of
  "+"   -> Op Plus     l r
  "*"   -> Op Mult     l r
  "="   -> Op Equal    l r
  "<"   -> Op LessThan l r
  "-"   -> Op Plus l (Op Mult r (NumE (-1)))
  "and" -> If l r FalseE
  "or"  -> If l TrueE r
  "<="  -> If (Op LessThan l r) TrueE (Op Equal l r)
  ">="  -> If (Op LessThan r l) TrueE (Op Equal l r)
  ">"   -> Op LessThan r l

parseString :: String -> Expr
parseString s = case runParser pExpr s of
  [(x,"")] -> x
  _        -> error "*** not parsable"

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

opTable :: Operator -> Value -> Value -> Value
opTable Plus     (NumV x) (NumV y) = NumV  (x + y)
opTable Mult     (NumV x) (NumV y) = NumV  (x * y)
opTable LessThan (NumV x) (NumV y) = BoolV (x < y)
opTable Equal    x        y        = BoolV (x == y)
opTable op x y = error ("*** " ++ show op ++ "is incompatible with "
                               ++ show x ++ " and " ++ show y)

interp :: Env -> Expr -> Value
interp env exp = case exp of
  NumE n -> NumV n

  TrueE -> BoolV True
  FalseE -> BoolV False

  Op op x y -> opTable op (interp env x) (interp env y)

  If cnd thn els -> case interp env cnd of
    BoolV True -> interp env thn
    BoolV False -> interp env els

  FnDef param body -> (FnV param body env)
  FnApp fn arg ->
    case interp env fn of 
      FnV param body closure -> 
        interp (local env param arg) body
      _  -> error "*** not a function"
    
        
  Id x -> lookupEnv x env

local :: Env -> [Identifier] -> [Expr] -> Env
local env (id:ids) (arg:args) = local ((id, interp env arg):env) ids args 
local env [] [] = env 
local env [] _ = error "*** not a function"
local env _ [] = error "*** not a function"

lookupEnv :: Identifier -> Env -> Value
lookupEnv x [] = error ("*** unbound variable " ++ x)
lookupEnv x ((id,val):bs)
  | x == id   = val
  | otherwise = lookupEnv x bs