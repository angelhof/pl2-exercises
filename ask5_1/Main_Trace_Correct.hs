import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Control.Arrow as Ar
import Data.Maybe

import Debug.Trace


-- TODO:
-- 1. (DONE) Make a replace function that searches the whole tree
--    for a specific variable and replaces it with a replacement
--    ast.
-- 2. (DONE) Call the replace function in the application command
-- 3. (DONE) Everywhere else continue as normal.
-- 4. Problem persists: 
--    * MAKE SURE THE PROBLEM IS WHAT SEEMS AND NOTHING ELSE
--    * Before trying the following solution check if cleanNames
--      works as expected
--    * I am guessing that the reason is that we need a clearer 
--      environment for variables. 
--    * I should try to solve this by changing replace so that it
--      also changes the numbers of the variables in the expressions
--      that it replaces by using cleanNames.
--    * This way we will achieve a no name conflict 
-- 4. Dont forget to test both with some more test cases.


-- Main function
main =  do  program <- getLine
            input <- getLine 
            putStrLn $ prepare program input 

-- Prepares the input and call the functions pipeline
prepare program input =  reverse output
  where
    ast = parseString program
    (cnt, clean_ast) = trace (show $ cleanNames ast 0) cleanNames ast 0
    (res, _in, output, _cnt) = execute clean_ast input cnt -- trace (show clean_ast) execute clean_ast input


cleanNames ast init = cleanNames' init Map.empty ast
  where
    cleanNames' cnt mapper (Cn con) = 
      (cnt, Cn' con)
    cleanNames' cnt mapper (Out expr1 expr2) = 
      (cnt2, Out' expr1' expr2')
        where
          (cnt1, expr1') = cleanNames' cnt mapper expr1
          (cnt2, expr2') = cleanNames' cnt1 mapper expr2
    cleanNames' cnt mapper (In expr) = 
      (cnt', In' expr')
        where
          (cnt', expr') = cleanNames' cnt mapper expr
    cleanNames' cnt mapper (Var var) = 
      (cnt, Var' res)
        where
          res = fromMaybe
            (error "Variable not found!!")
            (Map.lookup var mapper)
    cleanNames' cnt mapper (Fn var body) = 
      (cnt2, Fn' (cnt+1) res)
        where
          (cnt2, res) = cleanNames' (cnt+1) (Map.insert var (cnt+1) mapper) body
    cleanNames' cnt mapper (App expr1 expr2) = 
      (cnt2, App' expr1' expr2')
        where
          (cnt1, expr1') = cleanNames' cnt mapper expr1
          (cnt2, expr2') = cleanNames' cnt1 mapper expr2     


cleanNamesNew ast init = cleanNamesNew' init Map.empty ast
  where
    cleanNamesNew' cnt mapper (Cn' con) = 
      (cnt, Cn' con)
    cleanNamesNew' cnt mapper (Out' expr1 expr2) = 
      (cnt2, Out' expr1' expr2')
        where
          (cnt1, expr1') = cleanNamesNew' cnt mapper expr1
          (cnt2, expr2') = cleanNamesNew' cnt1 mapper expr2
    cleanNamesNew' cnt mapper (In' expr) = 
      (cnt', In' expr')
        where
          (cnt', expr') = cleanNamesNew' cnt mapper expr
    cleanNamesNew' cnt mapper (Var' var) = 
      (cnt, Var' res)
        where
          res = case Map.lookup var mapper of 
            Nothing -> var
            Just number -> number
    cleanNamesNew' cnt mapper (Fn' var body) = 
      (cnt2, Fn' (cnt+1) res)
        where
          (cnt2, res) = cleanNamesNew' (cnt+1) (Map.insert var (cnt+1) mapper) body
    cleanNamesNew' cnt mapper (App' expr1 expr2) = 
      (cnt2, App' expr1' expr2')
        where
          (cnt1, expr1') = cleanNamesNew' cnt mapper expr1
          (cnt2, expr2') = cleanNamesNew' cnt1 mapper expr2     


execute ast input cnt = execute' ast (Map.empty, input, "", cnt)
  where
    execute' (Cn' con) state@(env, input, out, cnt) = 
      (V con, input, out, cnt)
    execute' (Out' expr1 expr2) state@(env, input, out, cnt) = 
      trace (
        "Output: " ++ show (Out' expr1 expr2) ++
        "\n  Expr1: " ++ show expr1 ++
        "\n  Output: " ++ show (V o1)) 
        (res, input2, output2, cnt2)
        where
          (V o1, input1, output1, cnt1) = eval_v_if_needed expr1 state 
          (res, input2, output2, cnt2) = execute' expr2 (env, input1, o1:output1, cnt1)
    execute' (In' expr) state@(env, (ch:rest), output, cnt) = 
      trace (
        "Input: " ++ show (In' expr) ++
        "\n  New map: " ++ show env') 
        result
        where
          ((F (Fn' var body) mapper), input1, output1, cnt1) = 
              eval_f_if_needed expr (env, rest, output, cnt)
          result = execute' body (env', input1, output1, cnt1)
          env' = Map.insert var (V ch) mapper
    execute' (Var' var) state@(env, input, output, cnt) = 
      (res, input, output, cnt)
        where
          res = case Map.lookup var env of
            Nothing  -> error $ " !!!!!! -- Variable: " ++ show var ++ " not found in: " ++ show env
            Just (U (Var' var)) -> error "HIGH PROBLEM"
            Just val -> val
    execute' (Fn' var body) state@(env, input, output, cnt) = 
      result
        where
          result = (F (Fn' var body) env, input, output, cnt)  
    execute' (App' expr1 expr2) state@(env, input, output, cnt) = 
      trace (
        "Application: " ++ show (App' expr1 expr2) ++
        "\n  F1: " ++ show (Fn' var body) ++
        "\n  New Body: " ++ show body' ++  
        "\n  Add to map: " ++ show var ++ "\t" ++ show expr2) 
        result
        where
          (F (Fn' var body) mapper, input1, output1, cnt1) = eval_f_if_needed expr1 state 
          (body', cnt1') = replace var expr2 body cnt
          result = execute' body' (env', input1, output1, cnt1')
          env' = Map.insert var (U expr2) mapper
    eval_f_if_needed expr1 state@(e, i, o, cnt) = res
      where
        (ev_expr1, input1, output1, cnt1) = execute' expr1 state
        res = case ev_expr1 of
          F (Fn' var body) mapper -> (F (Fn' var body) mapper, input1, output1, cnt1) 
          U expr2 -> execute' expr2 (e, input1, output1, cnt1)
    eval_v_if_needed expr1 state@(e, i, o, cnt) = 
      trace ("Var: " ++ show expr1 ++ "\n  " ++ show state) res
      where
        (ev_expr1, input1, output1, cnt1) = execute' expr1 state
        res = case ev_expr1 of
          V o1 -> (V o1, input1, output1, cnt1) 
          U expr2 -> eval_v_if_needed expr2 (e, input1, output1, cnt1)

replace var exp ast cnt = (final_ast, final_cnt)
  where
    (final_ast, final_expr, final_cnt) = replace' var clean_expr ast cnt'
    (cnt', clean_expr) = cleanNamesNew exp (cnt+1)
    replace' var expr (Var' ast) cnt 
      | var == ast = (expr, expr', new_cnt)
      | otherwise = (Var' ast, expr, cnt)
        where
          (new_cnt, expr') = cleanNamesNew expr (cnt+1)
    replace' v e (Cn' char) cnt = 
      (Cn' char, e, cnt)
    replace' v e (Out' exp1 exp2) cnt = 
      (Out' res1 res2, e'', cnt'')
        where
          (res1, e', cnt') = replace' v e exp1 cnt
          (res2, e'', cnt'') = replace' v e' exp2 cnt'
    replace' v e (In' exp1) cnt = 
      (In' res1, e', cnt')
        where
          (res1, e', cnt') = replace' v e exp1 cnt
    replace' v e (Fn' var exp1) cnt = 
      (Fn' var res1, e', cnt')
        where
          (res1, e', cnt') = replace' v e exp1 cnt
    replace' v e (App' exp1 exp2) cnt = 
      (App' res1 res2, e'', cnt'') 
        where
          (res1, e', cnt') = replace' v e exp1 cnt
          (res2, e'', cnt'') = replace' v e' exp2 cnt'


---------------------
-- Data Structures --
---------------------

data Expr = Cn  Char
          | Out Expr Expr
          | In  Expr
          | Var Char
          | Fn  Char Expr
          | App Expr Expr

data Expr' = Cn'  Char
           | Out' Expr' Expr'
           | In'  Expr'
           | Var' Int
           | Fn'  Int Expr'
           | App' Expr' Expr'



data Value = V Char
           | F Expr' (Map.Map Int Value)
           | U Expr'
             deriving (Show)

--------------
-- Auxiliary 
--------------

instance Show Expr' where
  showsPrec p (Cn' c) =
    ('#' :) . (c :)
  showsPrec p (Out' e1 e2) =
    ('+' :) . showsPrec p e1 . showsPrec p e2
  showsPrec p (In' e) =
    ('-' :) . showsPrec p e
  showsPrec p (Var' x) =
    (show x ++)
  showsPrec p (Fn' x e) =
    ('/' :) . (show x ++) . showsPrec p e
  showsPrec p (App' e1 e2) =
    ('@' :) . showsPrec p e1 . showsPrec p e2


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
