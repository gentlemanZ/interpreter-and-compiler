{-Tianyang Zhong
 U19 28 5500 
 tianyang@bu.edu
 Validate
 cooperate with 
 Zheming Sun
 Yuming Zhu
 Yehui Huang
-}
module Validate where

import AbstractSyntax
import Interpret

-- Functions for generating key-value stores for testing
-- purposes given a collection of integers and keys.
dat :: [[String]] -> [Integer] -> KeyValueStore
dat (ks:kss) ns = [(k:ks', n) | (ks', n) <- dat kss ns, k <- ks]
dat []       ns = [([], Number n) | n <- ns]

dats :: [[String]] -> [Integer] -> [KeyValueStore]
dats kss ns = [dat (rotate i kss) (rotate j ns) | i <- [0..length kss-1], j <- [0..length ns-1]]
  where rotate i xs = take (length xs) (drop i (cycle xs))

class Exhaustive a where
  exhaustive :: Integer -> [a]

instance Exhaustive Exp where
  exhaustive 0 = []
  exhaustive 1 = [DATA, Variable "x", Variable "y"]  
  exhaustive n = exhaustive 1 ++ 
                [Max x1          | x1 <- exhaustive (n-1)] ++ 
                [Min x2          | x2 <- exhaustive (n-1)] ++ 
                [Sum x3          | x3 <- exhaustive (n-1)] ++ 
                [Product x4      | x4 <- exhaustive (n-1)] ++ 
                [Union x5        | x5 <- exhaustive (n-1)] ++ 
                [Intersection x6 | x6 <- exhaustive (n-1)] ++ 
                [MakeSet x7      | x7 <- exhaustive (n-1)] 


--instance Exhaustive Exp where
--	exhaustive 0 = []
--	exhaustive 1 = [Variable "x", Variable "y", DATA]
--	exhaustive n = 
--		nud([Max a|a <- exhaustive (n-1), a/=DATA] ++ 
--		[Min a|a <- exhaustive (n-1), a/=DATA] ++
--		[Sum a|a <- exhaustive (n-1), a/=DATA] ++
--		[Product a|a <- exhaustive (n-1), a/=DATA] ++
--		[Union a|a <- exhaustive (n-1), a/=DATA] ++
--		[Intersection a|a <- exhaustive (n-1), a/=DATA] ++ 
--		[MakeSet a|a <- exhaustive (n-1), a/=DATA] ++ [DATA])
  -- Complete for Problem 4, part (a).

instance Exhaustive Stmt where
  exhaustive 0 = []
  exhaustive 1 = [Return "x", Return "y"]
  exhaustive n = let rst = exhaustive (n - 1) :: [Stmt] in
                 let rep = exhaustive (n - 1) :: [Exp]  in
                   concat [ [Assign "x" e s, Assign "y" e s] | e <- rep, s <- rst ] ++ rst
  --exhaustive n = concat [[Assign "x" e s, Assign "y" e s] | e <- exhaustive (n - 1) :: [Stmt],s <- exhaustive (n - 1) :: [Exp]] ++ (exhaustive (n - 1) :: [Stmt])



validateHelper :: Stmt -> (Stmt -> Algorithm) -> (Stmt -> Algorithm) -> [KeyValueStore] -> [(Stmt, KeyValueStore)] -> [(Stmt, KeyValueStore)] 
validateHelper st f g [] keep    = keep
validateHelper st f g (k:kvs) keep = if (f st k) == (g st k) then (validateHelper st f g kvs keep) 
                                   else validateHelper st f g kvs (keep ++ [(st,k)])

validate :: Integer -> (Stmt -> Algorithm) -> (Stmt -> Algorithm) -> [KeyValueStore] -> [(Stmt, KeyValueStore)]
validate n f g [] = []
validate n f g kvs = let listOfSt = exhaustive n :: [Stmt] in 
                       concat [validateHelper st f g kvs [] | st <- listOfSt]


--eof