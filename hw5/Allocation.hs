---------------------------------------------------------------------
--
-- CAS CS 320, Spring 2015
-- Assignment 5 (skeleton code)
-- Allocation.hs
--

module Allocation where

type Item = Integer
type Bin = Integer

data Alloc = Alloc Bin Bin deriving (Eq, Show)
data Graph a =
    Branch a (Graph a) (Graph a) 
  | Finish a
  deriving (Eq, Show)


-- some helpers that will help us do some BinAlgebra
inBin :: Bin -> Item -> Bin
inBin  a  b = a+b

addBin:: Bin -> Bin ->Integer
addBin a b = a+b

-- the first graph fuction.
graph :: Alloc -> [Item] -> Graph Alloc
graph (Alloc a b) [x] = Branch (Alloc a b ) 
  (Finish (Alloc (inBin a x) b)) 
  (Finish (Alloc a    (inBin b x)))
graph (Alloc a b) ( x : xs) = Branch (Alloc a b)
  (graph (Alloc (inBin a x) b) (xs))
  (graph (Alloc a (inBin b x)) (xs))
-- contents function. 2 b
contents:: Graph a ->a
contents (Finish a) = a 
contents (Branch a (b) (c)) = a

-- 2 c
instance Ord a => Ord (Graph a) where
  a< a' = contents(a) < contents(a')
  a<= a' = contents(a) <= contents(a')
--2 d
instance Ord Alloc where
  Alloc a b < Alloc c d = abs(a-b) < abs(c-d)
  Alloc a b <= Alloc c d = abs(a-b) <= abs(c-d)
--2 e
final:: Graph a -> [a]
final (Branch a (b) (c)) = (final b) ++(final c)
final (Finish a) = [a]

--2 f
depth :: Integer -> Graph a -> [a]
depth n (Finish a )         = if n==0 then[a] else[]
depth n (Branch a (b) (c))  = if n<0 then [] else if n ==0 then [a] else (depth (n-1) (b)) ++ (depth (n-1) (c))

type Strategy = Graph Alloc -> Graph Alloc
--Next we will implement some strategies for the questions
--3 a Gready:
greedy :: Strategy
greedy (Finish a) =Finish a
greedy (Branch a (b) (c)) = if (contents b) < (contents c) then b else c
--3 b patient:
patient:: Integer -> Strategy
patient n (Branch a (b) (c)) =
  if n ==0 then (Branch a (b) (c)) else if (minimum (depth (n-1) (b))) <= (minimum (depth (n-1) (c))) 
  then patient (n-1) (b) else patient (n-1) (c)
patient 0 (Finish a) =Finish a
-- 3 c optimal:

optimal:: Strategy
optimal (Finish a) = Finish a
optimal (Branch a (b) (c)) = Finish (minimum(final(Branch a (b) (c))))
--3 d metaCompose:
metaCompose:: Strategy->Strategy->Strategy
(metaCompose a b )(c) = b(a(c))
--3 e metaRepeat
metaRepeat :: Integer -> Strategy -> Strategy
(metaRepeat 0 s)(g) = g
(metaRepeat n s)(g) = if n>0 then (metaRepeat (n-1) s)(s(g)) else (g)

--metaGreedy
metaGreedy :: Strategy -> Strategy -> Strategy
(metaGreedy s s2)(g) = if (s(g)) < (s2(g)) then (s(g)) else (s2(g))
--impatient:
impatient :: Integer -> Strategy
impatient n g = (metaRepeat n greedy) g
{-response to the last question:
  It is like a question about gameTree. patient is a strategy that go deap into the
  the tree and look at the node there, then do the comparison. impatient is a strategy
  that only look at the contents of the next level then do the comparsion. So, for 
  patient, assume input n the time complisity is O(n**4). For impatient it is O(n).
  THen we can see that impatient is much more efficient than patient. However, since 
  it only look at the next level of node, sometimes it can return the best choice of 
  two children. If we are looking for the best choice, we should use patient, but if 
  we really need to go very deap into the tree. we should use impatient. I think we can
  also use alph-bate branch cutting to improve the patient strategy just like what we do
    in a gametree.
-}


--eof