{-Tianyang Zhong
 U19 28 5500 
 tianyang@bu.edu
 AbstractSyntax
 cooperate with 
 Zheming Sun
 Yuming Zhu
 Yehui Huang
-}
module AbstractSyntax where
import Data.List (union, intersect, nub)

data Exp =
    DATA
  | Variable String
  | Max Exp
  | Min Exp
  | Sum Exp
  | Product Exp
  | Union Exp
  | Intersection Exp
  | MakeSet Exp
  deriving (Eq, Show)

data Stmt =
    Assign String Exp Stmt
  | Return String
  deriving (Eq, Show)

data Type =
    TyNumber
  | TySet
  | TyVoid
  deriving (Eq, Show)

data Value =
    Set [Integer]
  | Number Integer
  | Error
  deriving (Eq, Show)


instance Num Value where
 (Number a) * (Number b) = Number (a*b)
 (Set b)* (Number a ) =Error
 (Number a1 ) * (Set b) = Error
 (Number a) + (Number b) = Number (a+b)
 (Set _)  +(Number a ) =Error
 (Number a )+(Set _) = Error
 
instance Ord Value where
  Number a < Number b = a < b
  Set a < Number b = True
  Number a <= Number b = a <= b
  Set a <= Number b = True
(\/) :: Value -> Value -> Value
(\/) (Set a) (Set b) = Set (union a b)
(\/) (Number a)  _ = Error
(\/) _ (Number a) =  Error

(/\) :: Value -> Value -> Value
(/\) (Set a)  (Set b) = Set (intersect a b)
(/\) _ (Number a) =Error
(/\)(Number a) _ =Error
-- Type class Foldable for a fold function on data types.
--
--  * The first argument is a constant that will replace all
--    leaf nodes that contain no variable.
--  * The second argument is a function that will be applied to
--    (i.e., and will replace) any variables.
--  * The third argument is the aggregator for combining
--    results of recursive folds.
--  * The fourth argument is the data value that will be folded.

class Foldable a where
  fold :: b -> (String -> b) -> ([b] -> b) -> a -> b

instance Foldable Exp where
  --fold b v f _ = b
  fold b v f (Variable a) = v a 
  fold b v f (Max exp) = f [(fold b v f exp)]
  fold b v f (Min exp) = f [(fold b v f exp)]
  fold b v f (Sum exp) = f [(fold b v f exp)]
  fold b v f (Product exp) = f [(fold b v f exp)]
  fold b v f (Union exp) = f [(fold b v f exp)]
  fold b v f (Intersection exp) = f [(fold b v f exp)]
  fold b v f (MakeSet exp) = f [(fold b v f exp)]

instance Foldable Stmt where
  fold b v f (Return string) = v string -- Complete for Problem 1, part (d).
  fold b v f (Assign s e st) = f[(v s),(fold b v f e),(fold b v f st)]

vars :: Stmt -> [String]
vars st = nub (fold [] (\x ->[x]) (concat) st)


len :: [a] -> Integer
len [] = 0
len (_:xs) = 1+ len xs

operationTolist :: String -> [String]
operationTolist str = if str == "Max" || str == "Min" || str == "Sum" || str == "Product" || str == "Union" || str == "Intersection" || str == "MakeSet" 
  then [str]
  else []


operations :: Stmt -> Integer
operations st = len(fold [] (operationTolist) concat st) 
-- Complete for Problem 1, part (e).

-- eof