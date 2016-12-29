import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Control.Arrow as Ar

import Debug.Trace

--------------------
-- Main Functions --
--------------------


-- Finds all the rules that have to apply and annotates the ast
find_rules :: Expr -> Map.Map String Int -> (AExpr, [(Rule,Rule)])
find_rules ast variable_map = (annot_ast, rules_set)
  where
    (annot_ast, rules_set, _cnt) = findRules ast variable_map 0

findRules :: Expr -> Map.Map String Int -> Int -> (AExpr, [(Rule,Rule)], Int)
findRules (Id name) variable_map cnt =
  case Map.lookup name variable_map of
    Nothing -> (Id' "error" (-1), [], -1) 
    Just num -> (Id' name num, [], cnt)
findRules (Fun name body) variable_map cnt = res
  where
    res = case cnt' of 
      -1 -> (Id' "error" (-1), [], -1) 
      _  -> (Fun' name (cnt+1) annot_body (cnt'+1), rules', cnt'+1)
    rules' =  (Tp (cnt'+1), Paren (Tp (cnt+1)) (Tp (getType annot_body)) ):rules
    (annot_body, rules, cnt') = findRules body (Map.alter add_or_update name variable_map) (cnt+1)
    add_or_update _ = Just (cnt+1)
findRules (App exp1 exp2) variable_map cnt = res
  where
    res = case cnt1 == -1 || cnt2 == -1 of
      True ->  (Id' "error" (-1), [], -1)
      False -> (App' annot_exp1 cnt1 annot_exp2 (cnt2+1), (rule:rules1) ++ rules2, cnt2+1)
    rule = (Tp (getType annot_exp1), Paren (Tp (getType annot_exp2)) (Tp (cnt2+1)) )
    (annot_exp1, rules1, cnt1) = findRules exp1 variable_map cnt
    (annot_exp2, rules2, cnt2) = findRules exp2 variable_map cnt1


-- Unifies Rules into the final replacements - if possible
unify :: [(Rule, Rule)] -> [(Rule, Rule)] -> Maybe [(Rule, Rule)]
unify [] replacements = Just replacements
unify ((Paren a b, Paren c d):xs) repl = 
  unify ((a,c):(b,d):xs) repl
unify ((Tp a, r2):xs) repl
  | not (partOf (Tp a) r2) = unify (replaceAllWith (Tp a) r2 xs) ((Tp a,r2):repl) 
unify ((r1, Tp b):xs) repl
  | not (partOf (Tp b) r1) = unify (replaceAllWith (Tp b) r1 xs) ((Tp b,r1):repl) 
unify ((r1, r2):xs) repl 
  | equal r1 r2 = unify xs repl 
  | otherwise = Nothing


-- Replaces all occurences of r1 with r2 in a list of rules
replaceAllWith :: Rule -> Rule -> [(Rule, Rule)] -> [(Rule, Rule)] 
replaceAllWith r1 r2 = map (replace r1 r2 Ar.*** replace r1 r2)
  where 
    replace (Tp a) r2 (Tp leaf) 
      | a == leaf = r2
      | otherwise = Tp leaf
    replace (Tp a) r2 (Paren s1 s2) = 
      Paren (replace (Tp a) r2 s1) (replace (Tp a) r2 s2)
    replace (Paren a b) r2 (Paren s1 s2)
      | equal (Paren a b) (Paren s1 s2) = r2
      | otherwise = 
        Paren (replace (Paren a b) r2 s1) (replace (Paren a b) r2 s2)
    replace _ _ tree = tree


-- Finds the type of the total expression
finalType :: Int -> [(Rule, Rule)] -> Rule
finalType num replacements = rec_find (Tp num) all_mappings 
  where
    all_mappings = foldl replace mapper replacements
    mapper = Map.fromList (zip [1..num] (map Tp [1..num]))
    replace rep_map (Tp a, r2) = Map.insert a r2 rep_map
    rec_find (Tp a) all_mappings = if rep == Tp a then Tp a else rec_find rep all_mappings
      where
        rep = Map.findWithDefault (Tp a) a all_mappings
    rec_find (Paren r1 r2) _ = Paren (rec_find r1 all_mappings) (rec_find r2 all_mappings)


-- Simplifies and sorts the resulting types 
simplifyType :: Rule -> Int -> Map.Map Int Int -> Maybe Rule  
simplifyType (Tp (-1)) _ _ = Nothing
simplifyType tree cnt mapper = Just (fst' (simplifyType' tree cnt mapper))
  where
    simplifyType' (Tp a) cnt mapper = 
      case Map.lookup a mapper of
        Nothing  -> (Tp cnt, cnt+1, Map.insert a cnt mapper)
        Just num -> (Tp num, cnt, mapper)
    simplifyType' (Paren a b) cnt mapper = (Paren res1 res2, cnt2, mapper2)
      where
        (res1, cnt1, mapper1) = simplifyType' a cnt mapper
        (res2, cnt2, mapper2) = simplifyType' b cnt1 mapper1  
    fst' (x, _, _) = x


-- Pretty prints the type tree :)
prPrint :: Maybe Rule -> String 
prPrint Nothing     = "type error"
prPrint (Just tree) = prPrint' tree False
  where
    prPrint' (Tp a) _ = '@':(show a) 
    prPrint' (Paren a b) False = prPrint' a True ++ " -> " ++ prPrint' b False
    prPrint' (Paren a b) True = '(':(prPrint' a True) ++ " -> " ++ prPrint' b False ++ ")"
    

-- Main function
main =  do  input <- getContents 
            putStr . unlines . prepare $ input

-- Prepares the input and call the functions pipeline
prepare input =  pretty_print 
  where
    expressions = tail . lines $ input
    asts = map parseString expressions
    annot_asts_and_rules = map (`find_rules` Map.empty) asts
    replacements = map (\x -> unify (snd x) []) annot_asts_and_rules
    finalTypes = zipWith (curry filter_errors) annot_asts_and_rules replacements
      where 
        filter_errors ((x,_y), Just ls) = finalType (getType x) ls
        filter_errors ((_x,_y), Nothing) = Tp (-1)
    simple_types = map (\x -> simplifyType x 0 Map.empty) finalTypes
    pretty_print = map prPrint simple_types 

---------------------
-- Data Structures --
---------------------

data Expr = Id String
          | Fun String Expr
          | App Expr Expr
            deriving (Show)

data AExpr = Id' String Int
           | Fun' String Int AExpr Int
           | App' AExpr Int AExpr Int
             deriving (Show) 

data Rule = Tp Int
          | Paren Rule Rule
            deriving (Show, Eq)

--------------
-- Auxiliary 
--------------

getType :: AExpr -> Int 
getType (Id' _name num) = num
getType (Fun' _ _ _ num) = num
getType (App' _ _ _ num) = num

equal :: Rule -> Rule -> Bool
equal (Tp a) (Tp b) = a == b
equal (Paren a b) (Paren c d) = equal a c && equal b d
equal _ _ = False

partOf :: Rule -> Rule -> Bool
partOf (Tp a) (Tp b) = 
  a == b
partOf (Paren a b) (Paren c d) =  
     partOf (Paren a b) c 
  || partOf (Paren a b) d 
  || equal a c && equal b d 
partOf (Tp a) (Paren c d) = 
     partOf (Tp a) c 
  || partOf (Tp a) d
partOf _ _ = 
  False


--------------
--  Lexer
--------------

languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedOpNames = [ "\\"
                                     , "."
                                     ]
           }
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reservedOp lexer -- parses a reserved name
parens     = Token.parens     lexer


--------------
--  Parser 
--------------

expression :: Parser Expr
expression =   idExpr
           <|> parens expression'

expression' :: Parser Expr
expression' =   funExpr
            <|> appExpr

idExpr :: Parser Expr
idExpr = 
  do ide <- identifier
     return $ Id ide

funExpr :: Parser Expr
funExpr =
  do reserved "\\"
     param <- identifier
     reserved "."
     body <- expression
     return $ Fun param body

appExpr :: Parser Expr
appExpr =
  do expr1 <- expression
     expr2 <- expression
     return $ App expr1 expr2

parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r


