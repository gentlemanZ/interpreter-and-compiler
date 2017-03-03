{-Tianyang Zhong
 U19 28 5500 
 tianyang@bu.edu
 Interpret
 cooperate with 
 Zheming Sun
 Yuming Zhu
 Yehui Huang
-}
module Interpret where
import AbstractSyntax
import KeyValueStore
import TypeCheck
lookingup' :: String -> [(String, KeyValueStore)] -> KeyValueStore
lookingup' c' (c:cvs) = if (fst c) == c' then (snd c) else lookingup' c' (cvs)

makeSet :: Value -> Value
makeSet Error      = Error
makeSet (Number n) = Set [n]
makeSet (Set ns)   = Set ns

type KeyValueStore = [([String], Value)]
type Algorithm = KeyValueStore -> Maybe KeyValueStore

eval :: [(String, KeyValueStore)] -> Exp -> Algorithm
-- Complete for Problem 3, part (d).
eval env (DATA) kvs= if kvs ==[]  then Nothing else Just kvs
eval env (Min e) kvs= 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (min) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (Max e) kvs= 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (max) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (Sum e) kvs = 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (+) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (Product e) kvs = 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (*) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (Union e) kvs = 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (\/) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (Intersection e) kvs = 
  let store =(eval env e kvs)
  in if store == Nothing then Nothing else 
  let result = (suffix (combine 1 (/\) (fromJust(store))))
  in if result ==[] then Nothing else Just result
eval env (MakeSet e) kvs = 
  let r = eval env (e) kvs
  in if r == Nothing then Nothing else
  Just ([(a, Set [b])| (a,Number b)<- fromJust(r)])
eval env (Variable str) kvs =
  let result = [k|(s',k) <- env,s' ==str]
  in if result ==[] then Nothing else Just(head result)



exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
-- Complete for Problem 3, part (d).
exec env (Return str) kvs = eval env (Variable str) kvs
exec env (Assign str e stm) kvs =  
  let store = eval env e kvs
  in if store == Nothing then Nothing else
  let env1  = (env ++ [(str, fromJust store)])
  in exec env1 stm kvs


typeCheckInterpret :: Stmt -> Algorithm
-- Complete for Problem 3, part (e).
typeCheckInterpret sta kvs =
  if (typeCheck [] sta) == Nothing then Nothing
  else exec [] sta kvs


example = [(["sky","hello","world"], Number 1),(["sky","hello","world"],Number 2),(["dear","yes","no"],Number (-1))]
--eof