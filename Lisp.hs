module Lisp where

import Parsing 

--------------------------------------
-- Datatypes
--------------------------------------

type Identifier = String 
type Location = Int
type Env = [(Identifier,Location)] 
type Store = [(Location, Value)] 
type Result = (Value, Store)

-- Dropping BoolV for now, just to simplify things
data Value = NumV Int 
           | FnV Identifier Expr Env 
           | BoxV Location
           deriving Show

-- Dropping conditionals (If) for now, just to simplify things
data Expr = Plus Expr Expr
          | Mult Expr Expr
          | Num Int 
          | FnDef Identifier Expr
          | FnApp Expr Expr 
          | Id Identifier 
          | Box Expr 
          | Unbox Expr 
          | SetBox Expr Expr 
          | Seq Expr Expr
          deriving Show

--------------------------------------
-- Parser
--------------------------------------

-- Updated the Parser to be able to parse box, unbox, and setbox instructions.
pExpr :: Parser Expr 
pExpr = pPlus <|> pMult <|> pFnApp <|> pFnDef <|> pBox <|> pUnbox
          <|> pSetBox <|> pBegin <|> pNum <|> pId
   where
      pPlus = pFunc "+" (Plus <$> pExpr <*> pExpr) 
      pMult = pFunc "*" (Mult <$> pExpr <*> pExpr) 
      pFnApp = pFunc "" (FnApp <$> pExpr <*> pExpr) 
      pBox = pFunc "box" (Box <$> pExpr) 
      pUnbox = pFunc "unbox" (Unbox <$> pExpr) 
      pSetBox = pFunc "set-box!" (SetBox <$> pExpr <*> pExpr) 
      pBegin = pFunc "begin" (Seq <$> pExpr <*> pExpr) 
      pId = Id <$> identifier 
      pFnDef = pFunc "lambda" (FnDef <$> identifier <*> pExpr) 
      pNum = Num <$> natural

pFunc :: String -> Parser Expr -> Parser Expr 
pFunc s p = do symbol "(" 
               symbol s 
               exp <- p 
               symbol ")" 
               return exp

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

interp :: Env -> Store -> Expr -> Result
interp env st exp = case exp of 
    Num n -> (NumV n, st) 
    Id x -> let loc = lookupEnv x env 
                val = lookupSt loc st 
             in (val, st) 

    Box exp -> let (val, st') = interp env st exp 
                   loc = malloc st' 
                   b = BoxV loc 
                   st'' = (loc,val)  : st' 
                in (b, st'')
    
    Unbox exp -> let (val, st') = interp env st exp 
                  in case val of 
                        BoxV loc -> (lookupSt loc st', st')
                        _        -> error "*** not a box"

    Seq exp1 exp2 -> 
      let (val1, st') = interp env st exp1 
       in interp env st' exp2 

    Plus exp1 exp2 -> 
      let (val1, st') = interp env st exp1 
          (val2, st'') = interp env st' exp2 
          sum  = binOp (+) val1 val2 
       in (sum, st'') 

    Mult exp1 exp2 -> 
      let (val1, st') = interp env st exp1 
          (val2, st'') = interp env st' exp2 
          prod  = binOp (*) val1 val2 
       in (prod, st'') 

    FnDef param body -> (FnV param body env, st) 

    SetBox boxExp newExp -> 
      let (boxVal, st') = interp env st boxExp 
       in case boxVal of 
            BoxV loc -> let (newVal, st'') = interp env st' newExp 
                            st''' = updateSt loc newVal st'' 
                         in (boxVal, st''')
            _        -> error "*** not a box"

    FnApp fn arg -> 
      let (fnVal, st') = interp env st fn 
       in case fnVal of 
            FnV param body closure -> 
              let (argVal,st'') = interp env st' arg 
                  loc = malloc st'' 
                  st''' = (loc, argVal) : st'' 
                  env' = (param,loc) : closure 
               in interp env' st''' body
            _                      -> error "*** not a function"

lookupEnv :: Identifier -> Env -> Location
lookupEnv x [] = error "*** unbound variable "
lookupEnv x (b:bs)
  | x == fst b = snd b
  | otherwise  = lookupEnv x bs 

lookupSt :: Location -> Store -> Value
lookupSt x [] = error "*** location doesn't exist "
lookupSt x (b:bs)
  | x == fst b  = snd b
  | otherwise = lookupSt x bs 

malloc :: Store -> Location
malloc []     = 0
malloc (b:_) = fst b + 1 

-- Added an updateSt function. It searches through the store,
-- if it finds the (location, value) pair it is looking for,
-- it will update the store with the new value.
updateSt :: Location -> Value -> Store -> Store 
updateSt _ _ [] = error "*** location not defined" 
updateSt loc val (b : bs) 
   | loc == fst b = (loc, val) : bs 
   | otherwise = b : updateSt loc val bs