{-# OPTIONS_GHC -O2 -optc-O2 -with-rtsopts=-M64m #-}
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Sq
import qualified Data.Foldable as Fl
import qualified Data.List as List
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as LC

------------------------
--                    --
--    Globals
--                    --
------------------------

-- Maximum number of nodes
maxNodes = 10001

-- Main DFS Function
dfs init_head preorder adj = second (dfs' init_head (-1) Set.empty)
  where
    dfs' head father_id set =
      let
        curr_id = lookupF head preorder 0
        set_t = Set.insert head set -- trace ("Node: " ++ show head ++ " visited: " ++ show (Set.insert head set)) (Set.insert head set)
      in
        case IntMap.lookup head adj of
          Nothing -> (curr_id, 0, set_t)
          Just ls -> (low, cnt, set') -- trace ("Node: " ++  show head ++ " with pid: " ++ show curr_id ++ " returns: " ++ show (low, cnt) ++ " visited: " ++ show (Set.insert head set)) 
            where 
              res = dfss' ls curr_id [] 0 father_id set_t
              lows = first res
              ze_low = minimum' lows
              low =  if ze_low == maxNodes then curr_id else ze_low
              cnt = second res + if predicate1 || predicate2 then 1 else 0
                where
                  predicate1 = head /= init_head && any (>= curr_id) lows 
                  predicate2 = head == init_head && length ls > 1 && any (> curr_id) lows
              set' = third res
    dfss' [] pre_id lows cnt pre_pre set = (lows, cnt, set) -- trace ("Node's " ++ show pre_id ++ " are merged in " ++ show (lows, cnt)) (lows, cnt, set)
    dfss' (x:xs) pre_id lows cnt pre_pre set 
      | curr_id == pre_pre || (curr_id > pre_id && Set.member x set)= dfss' xs pre_id lows cnt pre_pre set
      | curr_id < pre_id  = dfss' xs pre_id (curr_id:lows) cnt pre_pre set
      | otherwise = dfss' xs pre_id lows' cnt' pre_pre set'
      where
        curr_id = lookupF x preorder 0 -- trace ("Node: " ++ show pre_id ++ " Kids: " ++ show (x:xs))
        res = dfs' x pre_id set
        lows' = first res : lows 
        cnt' = cnt + second res
        set' = third res -- trace ("Visited: " ++ show (third res)) (third res)

-- Main function
main =  do  input <- LC.getContents 
            print (prepare input)

-- Prepares the input and call the main function
prepare input = dfs head pre edges
  where
    head = 1
    edges_t = map (map readIntBs . LC.words)  (tail (LC.lines input))
    edges_d = flatmap (\x -> let tup = tuplify2 x in [tup, (snd tup, fst tup)]) edges_t
    edges = groupBy'' edges_d -- IntMap.empty -- trace (show edges_d) Map.fromList (groupBy'' edges_d [])
    pre = IntMap.fromList (zip (preorder' 1 edges) [1..] ) -- trace (show (Map.fromList (zip (preorder 1 edges) [1..] ))) (Map.fromList (zip (preorder 1 edges) [1..] ))


---------------------------
--                       --
--    Library          
--                       -- 
---------------------------

-- Faster ReadInt
readIntBs :: LC.ByteString -> Int
readIntBs bs = case LC.readInt bs of
                Nothing -> -1
                Just (x, _) -> x

-- Minimum with default value
minimum' ls = minimum (maxNodes:ls) 

-- Third Element of Tuple
third :: (a,b,c) -> c
third (_,_,x) = x

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,x,_) = x

-- List to tuple
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

-- Looksup the Preorder Map
lookupF x preo_map def = IntMap.findWithDefault def x preo_map

-- Returns a list with the preorder of the nodes
preorder' root adj = Fl.toList (fst (preo root Set.empty))
  where
    preo root set = 
      let
        set' = Set.insert root set
      in
        case IntMap.lookup root adj of
          Nothing -> (Sq.singleton root, set')
          Just ls -> (root Sq.<| ls', set'') 
            where
              res = preos ls set'
              ls' = fst res
              set'' = snd res
    preos [] set = (Sq.empty, set)
    preos (x:xs) set 
      | Set.member x set = preos xs set
      | otherwise = (ls', set'')
      where
        res = preo x set 
        set' = snd res
        res' = preos xs set'
        ls' = fst res Sq.>< fst res'
        set'' = snd res'

-- Maps one element of the initial list to many
flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs
        
-- Groups tuples into one tuple with a list as a snd
groupBy'' :: [(IntMap.Key, Int)] -> IntMap.IntMap [Int]
groupBy'' = List.foldl' fillMap IntMap.empty 
  where
    fillMap res x = IntMap.alter add_or_update (fst x) res
      where
        add_or_update Nothing = Just [snd x]
        add_or_update (Just ls) = Just (snd x : ls)


------ TO DELETE  ------------------

-- Groups tuples into one tuple with a list as a snd
groupBy' [] res =  res
groupBy' (x:xs) res = groupBy' xs (IntMap.alter add_or_update (fst x) res)    
  where  
    add_or_update Nothing = Just [snd x]
    add_or_update (Just ls) = Just (snd x : ls)
