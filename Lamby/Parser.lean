import Lamby.AST

namespace Lamby

inductive Token where
  | lparen : Token  -- '('
  | rparen : Token  -- ')'
  | lambda : Token  -- '\' | 'λ'
  | dot    : Token  -- '.'
  | ident  : String → Token
  deriving Repr

/--
Lexer
-/
partial def scan (s : String) : Except String (List Token) :=
  let isIdenChar (c : Char) : Bool :=
    !c.isWhitespace && !['(', ')', '\\', 'λ', '.'].contains c

  let rec loop (cs : List Char) (acc : List Token) : Except String (List Token) :=
    let cleanCs := cs.dropWhile Char.isWhitespace
    match cleanCs with
    | [] => Except.ok acc.reverse
    | c :: rest =>
      match c with
      | '(' => loop rest (Token.lparen :: acc)
      | ')' => loop rest (Token.rparen :: acc)
      | '.' => loop rest (Token.dot :: acc)
      | '\\'
      | 'λ' => loop rest (Token.lambda :: acc)
      | _ =>
        let (idenChars, remaining) := cleanCs.span isIdenChar
        if idenChars.isEmpty then
          Except.error s!"Invalid character in input: '{c}'"
        else
          let token := Token.ident (String.ofList idenChars)
          loop remaining (token :: acc)

  loop s.toList []

#eval scan "\\x. (x y)"

#check String.ValidPos
