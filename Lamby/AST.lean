namespace Lamby

abbrev Name := String

/--
https://en.wikipedia.org/wiki/Lambda_calculus_definition#Lambda_terms
-/
inductive Term where
  | var : Name → Term
  | abs : Name → Term → Term
  | app : Term → Term → Term
