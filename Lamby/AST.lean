namespace Lamby

abbrev Name := String

/--
https://en.wikipedia.org/wiki/Lambda_calculus_definition#Lambda_terms
-/
inductive Term where
  | var : Name → Term
  | abs : Name → Term → Term
  | app : Term → Term → Term

-- x
#check Term.var "x"

-- ID: (λx.x)
#check Term.abs "x" (Term.var "x")

-- (x y)
#check Term.app (Term.var "x") (Term.var "y")
