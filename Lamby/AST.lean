namespace Lamby

abbrev Name := String

/--
https://en.wikipedia.org/wiki/Lambda_calculus_definition#Lambda_terms
-/
inductive Term where
  | var : Name → Term
  | abs : Name → Term → Term
  | app : Term → Term → Term

def Term.toString : Term → String
  | var n => n
  | abs n body => s!"(λ{n}. {body.toString})"
  | app t1 t2 => s!"({t1.toString} {t2.toString})"

instance : ToString Term where
  toString := Term.toString
