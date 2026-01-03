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

#eval unusedName ("x'" :: standardNames)
