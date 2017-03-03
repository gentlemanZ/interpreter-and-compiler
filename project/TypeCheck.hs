{-Tianyang Zhong
 U19 28 5500 
 tianyang@bu.edu
 TypeCheck
 cooperate with 
 Zheming Sun
 Yuming Zhu
 Yehui Huang
-}
module TypeCheck where

import AbstractSyntax
import Parse

fromJust :: Maybe a -> a
fromJust (Just x) = x

isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

lookup' :: String -> [(String, Type)] -> Type
lookup' c' (c:cvs) = if (fst c) == c' then (snd c) else lookup' c' (cvs)

class Typeable a where
  typeCheck :: [(String, Type)] -> a -> Maybe Type
  
instance Typeable Exp where 
-- Complete for Problem 2, part (a).
  typeCheck env (Min e) =
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TyNumber then
  		Just TyNumber  
  	  else Nothing

  typeCheck env (Max e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TyNumber then
  		Just TyNumber
  	  else Nothing

  typeCheck env (Sum e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TyNumber then
  		Just TyNumber
  	  else Nothing

  typeCheck env (Product e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
    if fromJust (v) == TyNumber then
  		Just TyNumber
  	  else Nothing

  typeCheck env ( MakeSet e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TyNumber then
  		Just TySet
  	  else Nothing

  typeCheck env (Union e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TySet then 
  		Just TySet
  	  else Nothing

  typeCheck env (Intersection e) = 
  	let v = typeCheck env e
    in if v ==Nothing then Nothing else
  	if fromJust (v) == TySet then 
  		Just TySet
  	  else Nothing

  typeCheck env (Variable x) = 
  	if length [v | (y, v) <- env, x == y] > 0 then
  		Just (lookup' x env)
  	else Nothing
  typeCheck env (DATA) = Just TyNumber



instance Typeable Stmt where 
-- Complete for Problem 2, part (b).
  typeCheck env (Assign x e s) = 
  	let v = typeCheck env e 
  	in if isJust v then
  		let env' = env ++ [(x, fromJust(v))]
  		in let v1 = typeCheck env' s
        in if v1 ==Nothing then Nothing else
  		if fromJust(v1) == TyVoid then
  			Just TyVoid
  		else Nothing
    else Nothing

  typeCheck env (Return x) = 
	if length ([x|(x,t)<- env])>0 then
		Just TyVoid
	else Nothing
	


liftMaybe :: (a -> b) -> (Maybe a -> Maybe b)
-- Complete for Problem 2, part (c).
liftMaybe f (Just a) = Just (f a)
liftMaybe f (Nothing) = Nothing


joinMaybe :: Maybe (Maybe a) -> Maybe a
-- Complete for Problem 2, part (c).
joinMaybe (Just(Just a)) = Just a
joinMaybe (Just Nothing) = Nothing
joinMaybe Nothing = Nothing

tokenizeParseTypeCheck :: String -> Maybe Type
--tokenizeParseTypeCheck s = joinMaybe(liftMaybe (typeCheck[]) (tokenizeParse (s)) )
tokenizeParseTypeCheck s =typeCheck [] (fromJust(tokenizeParse s) :: Stmt)

