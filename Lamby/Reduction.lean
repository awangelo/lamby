import Lamby.AST

namespace Lamby

/--
Returns the list of free variables in a term.
-/
def freeVars : Term → List Name
  | Term.var n => [n]
  | Term.app t1 t2 => (freeVars t1) ++ (freeVars t2)
  | Term.abs n body => (freeVars body).filter (· != n)

/--
List of variable names (in order of preference).
-/
def standardNames : List String :=
  ["x", "y", "z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
   "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w"]

/--
Returns an unused variable name, prioritizing standard names:
(`x, y, z, ...`, `x', y', ...`, `x'', y'', ...`).

Partial because there can be infinitely many variables.
-/
partial def unusedName (used : List Name) : Name :=
  let rec loop (suffix : String) :=
    match standardNames.find? (fun base => !used.contains (base ++ suffix)) with
    | some base => base ++ suffix
    | none => loop (suffix ++ "'")
  loop ""

/--
Replace all free occurrences of the variable `V` in the expression `E` with expression `R`
(E[V := R]).

Implements capture-avoiding substitution.
-/
partial def substitute (var : Name) (expr : Term) (replacement : Term) : Term :=
  let replacementFV := freeVars replacement

  let rec go (t : Term) : Term :=
    match t with
    | Term.var n =>
      -- Rule: x[x := N] = N; y[x := N] = y
      if n == var then replacement else Term.var n

    | Term.app m1 m2 =>
      -- Rule: (M1 M2)[x := N] = (M1[x := N]) (M2[x := N])
      Term.app (go m1) (go m2)

    | Term.abs y m =>
      if var == y then
        -- Rule: (λx.M)[x := N] = λx.M
        -- The variable x is bound, stop substitution.
        Term.abs y m
      else if replacementFV.contains y then
        -- Rule: (λy.M)[x := N] where y ∈ FV(N) and x ≠ y (would cause capture).

        -- α-convert λy.M to λz.M[y := z] and rename the body.
        let fv := (freeVars m) ++ replacementFV -- Forbidden names.
        let newName := unusedName fv
        let mRenamed := substitute y m (Term.var newName)

        -- Continue substitution in the new body.
        Term.abs newName (go mRenamed)
      else
        -- No capture risk (y ∉ FV(N)), continue substitution in body.
        Term.abs y (go m)

  go expr

/--
Reduces a term to its normal form using Normal Order strategy
(Leftmost-outermost reduction).
-/
partial def reduce : Term → Term
  | Term.var n => Term.var n

  | Term.abs n body => Term.abs n (reduce body)

  | Term.app (Term.abs x body) arg =>
    reduce (substitute x body arg)

  | Term.app t1 t2 =>
      let t1' := reduce t1
      match t1' with
      -- If t1 reduced, we have a new term (t1' t2), try reducing it again.
      | Term.abs _ _ => reduce (Term.app t1' t2)
      -- t1' is in normal form, try reducing t2.
      | _ => Term.app t1' (reduce t2)
