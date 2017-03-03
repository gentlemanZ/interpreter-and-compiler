---------------------------------------------------------------------
--
-- CAS CS 320, Spring 2015
-- Assignment 4 (skeleton code)
-- Tree.hs
--

data Tree =
    Leaf
  | Twig
  | Branch Tree Tree Tree
  deriving (Eq, Show);
-----------------------------------------------------------------------
twigs :: Tree -> Integer
twigs(Leaf) = 0;
twigs(Twig) = 1;
twigs(Branch t0 t1 t2) = twigs(t0)+twigs(t1)+twigs(t2);


branches :: Tree -> Integer
branches(Leaf) = 0;
branches(Twig)=0;
branches(Branch t0 t1 t2) = 1+branches(t0)+branches(t1)+branches(t2);


height :: Tree -> Integer
height(Leaf) = 0;
height(Twig) = 1;
height(Branch t0 t1 t2) = 1+maximum[height(t0),height(t1),height(t2)];

perfect :: Tree -> Bool
perfect(Leaf) = True;
perfect(Twig) = False;
perfect(Branch t0 t1 t2) = perfect(t0) && perfect(t1) && perfect(t2) && height(t0) == height(t1) && height(t1) == height(t2);

degenerate :: Tree -> Bool
degenerate(Leaf) = True;
degenerate(Twig) = True;
degenerate ( Branch Leaf Leaf Leaf ) = True;
degenerate ( Branch Twig Twig Twig ) = True;
degenerate ( Branch t Leaf Leaf ) = degenerate(t);
degenerate ( Branch t Leaf Twig ) = degenerate(t);
degenerate ( Branch t Twig Leaf ) = degenerate(t);
degenerate ( Branch t Twig Twig ) = degenerate(t);
degenerate ( Branch Leaf t Leaf ) = degenerate(t);
degenerate ( Branch Leaf t Twig ) = degenerate(t);
degenerate ( Branch Twig t Leaf ) = degenerate(t);
degenerate ( Branch Twig t Twig ) = degenerate(t);
degenerate ( Branch Leaf Leaf t ) = degenerate(t);
degenerate ( Branch Leaf Twig t ) = degenerate(t);
degenerate ( Branch Twig Leaf t ) = degenerate(t);
degenerate ( Branch Twig Twig t ) = degenerate(t);
degenerate ( Branch t t1 t2 ) = False;

infinite::Tree
infinite = Branch infinite infinite infinite
--eof