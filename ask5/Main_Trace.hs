import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Control.Arrow as Ar

import Debug.Trace


-- TODO:
-- =====
-- 1. Bres pou sumbainei to bug akribws tupwnontas to enviroment 
--    meta apo kathe sunarthsh 
-- 2. An den doulepsei katharograpse ton kodika kai xanadesto  

-- Main function
main =  do  program <- getLine
            input <- getLine 
            putStrLn $ prepare program input 

-- Prepares the input and call the functions pipeline
prepare program input =  show ast ++ "\n" ++ show (reverse output)
  where
    ast = parseString program
    (res, _in, output) = execute ast input

execute ast input = execute' ast (Map.empty, input, "")
  where
    execute' (Cn con) state@(env, input, out) = 
      trace (
        " -- Constant: \n\t" ++ show (Cn con) ++ 
        "\n  -- Result: " ++ show (Cn con, input, out) ++ 
        "\n\twith env= " ++ show env) 
        (V con, input, out)
    execute' (Out expr1 expr2) state@(env, input, out) = 
      trace (
        " -- Output: \n\t" ++ show expr1 ++ 
        "\n\t" ++ show expr2 ++ 
        "\n  -- Result: " ++ show (res, input2, output2) ++ 
        "\n\twith env= " ++ show env) 
        (res, input2, output2)
        where
          (V o1, input1, output1) = execute' expr1 state 
          (res, input2, output2) = execute' expr2 (env, input1, o1:output1)
    execute' (In expr) state@(env, (ch:rest), output) = 
      trace (
        " -- Input: \n\t" ++ show expr ++
        "\n  -- Result: " ++ show result ++ 
        "\n\twith env= " ++ show env') 
        result
        where
          ((F (Fn var body) mapper), input1, output1) = execute' expr (env, rest, output)
          result = execute' body (env', input1, output1)
          env' = Map.insert var (V ch) mapper
    execute' (Var var) state@(env, input, output) = 
      trace (
        " -- Variable: \n\t" ++ show var ++ 
        "\n  --Result: " ++ show (res, input, output) ++ 
        "\n\twith env= " ++ show env) 
        (res, input, output)
        where
          res = case Map.lookup var env of
            Nothing  -> error $ " !!!!!! -- Variable: " ++ show var ++ " not found in: " ++ show env
            Just val -> val
    execute' (Fn var body) state@(env, input, output) = 
      trace (
        " -- Function: \n\t" ++ show (Fn var body) ++ 
        "\n  -- Result: " ++ show result ++ 
        "\n\twith env= " ++ show env) result
        where
          result = (F (Fn var body) env, input, output)  
    execute' (App expr1 expr2) state@(env, input, output) = 
      trace (
        " -- Application: \n\t" ++ show expr1 ++ 
        "\n\t" ++ show expr2 ++ 
        "\n  -- Result: " ++ show result ++ 
        "\n\twith env= " ++ show env') result
        where
          (F (Fn var body) mapper, input1, output1) = execute' expr1 state
          (res2, input2, output2) = execute' expr2 (env, input1, output1)
          result = execute' body (env', input2, output2)
          env' = Map.insert var res2 mapper

---------------------
-- Data Structures --
---------------------

data Expr = Cn  Char
          | Out Expr Expr
          | In  Expr
          | Var Char
          | Fn  Char Expr
          | App Expr Expr

instance Show Expr where
  showsPrec p (Cn c) =
    ('#' :) . (c :)
  showsPrec p (Out e1 e2) =
    ('+' :) . showsPrec p e1 . showsPrec p e2
  showsPrec p (In e) =
    ('-' :) . showsPrec p e
  showsPrec p (Var x) =
    (x :)
  showsPrec p (Fn x e) =
    ('/' :) . (x :) . showsPrec p e
  showsPrec p (App e1 e2) =
    ('@' :) . showsPrec p e1 . showsPrec p e2

data Value = V Char
           | F Expr (Map.Map Char Value)
             deriving (Show)

--------------
-- Auxiliary 
--------------


--------------
--  Lexer
--------------

reservedChars = "#+-/@"

languageDef =
  emptyDef { Token.reservedOpNames = [ "#"
                                     , "+"
                                     , "-"
                                     , "/"
                                     , "@"
                                     ]
           }
lexer = Token.makeTokenParser languageDef

--------------
--  Parser 
--------------

expression :: Parser Expr
expression =   constantExpr
           <|> outputExpr
           <|> inputExpr
           <|> variableExpr
           <|> functionExpr
           <|> applicationExpr


constantExpr :: Parser Expr
constantExpr = 
  do char '#'
     constant <- noneOf reservedChars
     return $ Cn constant  

outputExpr :: Parser Expr
outputExpr = 
  do char '+'
     expr1 <- expression
     expr2 <- expression
     return $ Out expr1 expr2  

inputExpr :: Parser Expr
inputExpr = 
  do char '-'
     expr <- expression
     return $ In expr

variableExpr :: Parser Expr
variableExpr = 
  do var <- noneOf reservedChars
     return $ Var var

functionExpr :: Parser Expr
functionExpr = 
  do char '/'
     var <- noneOf reservedChars
     body <- expression
     return $ Fn var body

applicationExpr :: Parser Expr
applicationExpr = 
  do char '@'
     expr1 <- expression
     expr2 <- expression
     return $ App expr1 expr2


parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r
