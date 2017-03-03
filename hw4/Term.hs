---------------------------------------------------------------------
--
-- CAS CS 320, Spring 2015
-- Assignment 4 (skeleton code)
-- Term.hs
--

data Term =
    Number Integer
  | Abs Term
  | Plus Term Term
  | Mult Term Term

evaluate :: Term -> Integer
evaluate (Number x) = x;
evaluate (Abs x) =
    if evaluate(x) < 0
        then -evaluate(x)
        else evaluate(x)
evaluate (Plus x1 x2) = evaluate(x1) + evaluate(x2)
evaluate ( Mult x1 x2) = evaluate(x1) * evaluate(x2)

--eof