import Lamby.AST

namespace Lamby

inductive Token where
  | lparen : Token  -- '('
  | rparen : Token  -- ')'
  | lambda : Token  -- '\' | 'λ'
  | dot    : Token  -- '.'
  | ident  : String → Token

def Token.toString : Token → String
  | lparen => "("
  | rparen => ")"
  | lambda => "λ"
  | dot    => "."
  | ident s => s

instance : ToString Token where
  toString := Token.toString

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

/--
Either an error message or a parsed Term along with remaining tokens.
-/
def ParseResult := Except String (Term × List Token)

mutual
  partial def parseAtom (tokens : List Token) : ParseResult :=
    match tokens with
    -- 1. Variable: IDENTIFIER
    | Token.ident n :: rest =>
      Except.ok (Term.var n, rest)

    -- 2. Grouping: '(' Term ')'
    | Token.lparen :: rest =>
      match parseTerm rest with
      | Except.ok (t, Token.rparen :: restAfter) => Except.ok (t, restAfter)
      | Except.ok _ => Except.error "Expected ')' after grouped term"
      | Except.error e => Except.error e

    -- 3. Abstraction: 'λ' IDENTIFIER '.' Term
    | Token.lambda :: Token.ident n :: Token.dot :: rest =>
      match parseTerm rest with
      | Except.ok (body, restAfter) => Except.ok (Term.abs n body, restAfter)
      | Except.error e => Except.error e

    -- Syntax errors
    | Token.lambda :: Token.ident _ :: _ => Except.error "Expected '.' and a function body"
    | Token.lambda :: _ => Except.error "Expected an identifier after 'λ'"
    | [] => Except.error "Unexpected end of input while parsing an atom"
    | t :: _ => Except.error s!"Unexpected token \"{t}\" while parsing an atom"

  /--
  Reads a sequence of applications starting from an initial term.

  `a b c -> ((a b) c)`
  -/
  partial def parseTerm (tokens : List Token) : ParseResult :=
    match parseAtom tokens with
    | Except.error e => Except.error e
    | Except.ok (first, rest) => parseAppChain first rest

  /--
  Parses a chain of applications, left-associative.

  `left` is the term parsed so far, `tokens` are the remaining.
  -/
  partial def parseAppChain (left : Term) (tokens : List Token) : ParseResult :=
    -- Lookahead to see if there's another atom to parse.
    match parseAtom tokens with
    | Except.ok (right, restAfter) =>
      parseAppChain (Term.app left right) restAfter
    | Except.error _ =>
      Except.ok (left, tokens)
end

/--
Term ∷= Atom { Atom }  -- Application (left-associative)
Atom ∷= Variable       -- Variable
      | '(' Term ')'
      | 'λ' IDENTIFIER '.' Term
      | '\' IDENTIFIER '.' Term
-/
def parse (s : String) : Except String Term :=
  match scan s with
  | Except.error e => Except.error e
  | Except.ok tokens =>
    match parseTerm tokens with
    | Except.ok (t, []) => Except.ok t  -- No remaining tokens.
    | Except.ok (_, t :: _) => Except.error s!"Syntax error: unexpected token \"{t}\""
    | Except.error e => Except.error e
