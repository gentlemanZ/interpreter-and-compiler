---------------------------------------------------------------------
--
-- CAS CS 320, Spring 2015
-- Assignment 5 (skeleton code)
-- Database.hs
--

module Database where

type Column = String
data User = User String deriving (Eq, Show)
data Table = Table String deriving (Eq, Show)
data Command =
    Add User
  | Create Table
  | Allow (User, Table)
  | Insert (Table, [(Column, Integer)])
  deriving (Eq, Show)

example = [
    Add (User "Alice"),
    Add (User "Bob"),
    Create (Table "Revenue"),
    Insert (Table "Revenue", [("Day", 1), ("Amount", 2400)]),
    Insert (Table "Revenue", [("Day", 2), ("Amount", 1700)]),
    Insert (Table "Revenue", [("Day", 3), ("Amount", 3100)]),
    Allow (User "Alice", Table "Revenue")
  ]

-- Useful function for retrieving a value from a list
-- of (label, value) pairs.
lookup' :: Column -> [(Column, Integer)] -> Integer
lookup' c' (c:cvs) = if (fst c) == c' then (snd c) else lookup' c' (cvs)


 --Complete for Assignment 5, Problem 1, part (a).
select :: [Command] -> User -> Table -> Column -> Maybe [Integer]
select commands (User u) (Table t) col = 
  if (User u, Table t)`elem` [(x,y)|Allow(x,y)<-commands]&&(User u)`elem` [x|Add(x)<-commands] then
  Just [lookup' col a| Insert (Table t,a) <- commands] else
  Nothing
-- Type synonym for aggregation operators.
type Operator = Integer -> Integer -> Integer 

-- Complete Assignment 5, Problem 1, parts (b) and (c) here.
foldr' :: (Integer -> Integer-> Integer) -> Integer -> [Integer] -> Integer
foldr' f base []     = base
foldr' f base (x:xs) = f x (foldr f base xs)

aggregate :: [Command] -> User -> Table -> Column -> Operator -> Integer -> Maybe Integer
aggregate commands (User u) (Table t) col f b =
  if (User u, Table t)`elem` [(x,y)|Allow(x,y)<-commands]&&(User u)`elem` [x|Add(x)<-commands]  then
  Just (foldr' f b [lookup' col a| Insert (Table t,a) <- commands]) else
  Nothing

check:: [Command]-> [Command] ->Bool
check [(Allow (User a, Table b))] (xs) = if User a `elem` [d|Add(d)<- xs] && Table b `elem`[y|Create (y)<- xs] then True else False
  
check [(Insert (Table a, _))] (xs)= if Table a `elem` [y|Create (y)<- xs] then True else False
check [(Add (_))] (xs)=  True
--check Add (_) [] = True 
check [(Create(_))] (xs)= True
check [] (x:xs) = check [x] (xs) && check [] (xs)
check [] [] = True
 

validate::[Command] -> Bool
validate commands = check [] (reverse commands)
  
  --if [x|Insert (x,_) <- commands] == [y|Create (y)<- commands]
  -- && [z| Allow (_, z)<- commands] == [y|Create (y)<- commands]
  -- && [b| Allow (b, _)<- commands]  ==[d|Add(d)<- commands]
  --then True else False
  --validate Insert (Table a,_) = if a `elem` []
--eof