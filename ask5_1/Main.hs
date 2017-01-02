import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Map as Map
import Data.Maybe

-- Main function
main :: IO ()
main =  do  program <- getLine
            input <- getLine 
            putStrLn $ prepare program input 

-- Prepares the input and call the functions pipeline
prepare :: String -> [Char] -> [Char]
prepare program input =  reverse output
  where
    ast = parseString program
    (cnt, clean_ast) = cleanNames ast 0
    (_res, _in, output, _cnt) = execute clean_ast input cnt

execute :: Expr' -> [Char] -> Int -> (Value, [Char], [Char], Int)
execute ast init_input init_cnt = execute' ast (Map.empty, init_input, "", init_cnt)
  where
    execute' (Cn' con) (_env, input, out, cnt) = 
      (V con, input, out, cnt)
    execute' (Out' expr1 expr2) state@(env, _input, _out, _cnt) = 
      (res, input2, output2, cnt2)
        where
          (V o1, input1, output1, cnt1) = eval_v_if_needed expr1 state 
          (res, input2, output2, cnt2) = execute' expr2 (env, input1, o1:output1, cnt1)
    execute' (In' expr) state@(env, ch:rest, output, cnt) = 
      result
        where
          (F (Fn' var body) mapper, input1, output1, cnt1) = 
              eval_f_if_needed expr (env, rest, output, cnt)
          result = execute' body (env', input1, output1, cnt1)
          env' = Map.insert var (V ch) mapper
    execute' (Var' var) state@(env, input, output, cnt) = 
      (res, input, output, cnt)
        where
          res = fromMaybe
            (error $ 
              " !!!!!! -- Variable: " ++ 
                show var ++ " not found in: " ++ show env)
            (Map.lookup var env)
    execute' (Fn' var body) state@(env, input, output, cnt) = 
      result
        where
          result = (F (Fn' var body) env, input, output, cnt)  
    execute' (App' expr1 expr2) state@(env, input, output, cnt) = 
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
      res
        where
          (ev_expr1, input1, output1, cnt1) = execute' expr1 state
          res = case ev_expr1 of
            V o1 -> (V o1, input1, output1, cnt1) 
            U expr2 -> eval_v_if_needed expr2 (e, input1, output1, cnt1)



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
  showsPrec _ (Cn' c) =
    ('#' :) . (c :)
  showsPrec p (Out' e1 e2) =
    ('+' :) . showsPrec p e1 . showsPrec p e2
  showsPrec p (In' e) =
    ('-' :) . showsPrec p e
  showsPrec _ (Var' x) =
    (show x ++)
  showsPrec p (Fn' x e) =
    ('/' :) . (show x ++) . showsPrec p e
  showsPrec p (App' e1 e2) =
    ('@' :) . showsPrec p e1 . showsPrec p e2


instance Show Expr where
  showsPrec _ (Cn c) =
    ('#' :) . (c :)
  showsPrec p (Out e1 e2) =
    ('+' :) . showsPrec p e1 . showsPrec p e2
  showsPrec p (In e) =
    ('-' :) . showsPrec p e
  showsPrec _ (Var x) =
    (x :)
  showsPrec p (Fn x e) =
    ('/' :) . (x :) . showsPrec p e
  showsPrec p (App e1 e2) =
    ('@' :) . showsPrec p e1 . showsPrec p e2

replace :: Int -> Expr' -> Expr' -> Int -> (Expr', Int)
replace var init_expr ast init_cnt = (final_ast, final_cnt)
  where
    (final_ast, final_expr, final_cnt) = replace' var clean_expr ast cnt1
    (cnt1, clean_expr) = cleanNamesNew init_expr (init_cnt+1)
    replace' var expr (Var' ast) cnt 
      | var == ast = (expr, expr', new_cnt)
      | otherwise = (Var' ast, expr, cnt)
        where
          (new_cnt, expr') = cleanNamesNew expr (cnt+1)
    replace' v e (Cn' ch) cnt = 
      (Cn' ch, e, cnt)
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


cleanNames :: Expr -> Int -> (Int, Expr')
cleanNames ast init_cnt = cleanNames' init_cnt Map.empty ast
  where
    cleanNames' cnt _mapper (Cn con) = 
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

cleanNamesNew :: Expr' -> Int -> (Int, Expr')
cleanNamesNew ast init_cnt = cleanNamesNew' init_cnt Map.empty ast
  where
    cleanNamesNew' cnt _mapper (Cn' con) = 
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
          res = fromMaybe var $ Map.lookup var mapper
    cleanNamesNew' cnt mapper (Fn' var body) = 
      (cnt2, Fn' (cnt+1) res)
        where
          (cnt2, res) = cleanNamesNew' (cnt+1) (Map.insert var (cnt+1) mapper) body
    cleanNamesNew' cnt mapper (App' expr1 expr2) = 
      (cnt2, App' expr1' expr2')
        where
          (cnt1, expr1') = cleanNamesNew' cnt mapper expr1
          (cnt2, expr2') = cleanNamesNew' cnt1 mapper expr2     



--------------
--  Lexer
--------------

reservedChars :: [Char]
reservedChars = "#+-/@"

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
  do _ <- char '#'
     constant <- noneOf reservedChars
     return $ Cn constant  

outputExpr :: Parser Expr
outputExpr = 
  do _ <- char '+'
     expr1 <- expression
     expr2 <- expression
     return $ Out expr1 expr2  

inputExpr :: Parser Expr
inputExpr = 
  do _ <- char '-'
     expr <- expression
     return $ In expr

variableExpr :: Parser Expr
variableExpr = 
  do var <- noneOf reservedChars
     return $ Var var

functionExpr :: Parser Expr
functionExpr = 
  do _ <- char '/'
     var <- noneOf reservedChars
     body <- expression
     return $ Fn var body

applicationExpr :: Parser Expr
applicationExpr = 
  do _ <- char '@'
     expr1 <- expression
     expr2 <- expression
     return $ App expr1 expr2


parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r
