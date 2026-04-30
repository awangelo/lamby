import Lamby.Parser
import Lamby.Reduction

namespace Lamby

inductive Command where
  | help (toDescribe : Option String)
  | quit
  | unknown (cmd : String)

def parseCommand (input : String) : Command :=
  let parts := input.splitOn " " |>.filter (· != "")

  match parts with
  | [":quit"] | [":q"] => .quit
  | [":help"] | [":h"] => .help none
  | [":help", arg]
  | [":h", arg]        => .help (some arg)
  | cmd :: _           => .unknown cmd
  | []                 => .unknown ""

def evalExpression (input : String) : IO Unit := do
  match parse input with
  | .error err => .println s!"Error: {err}"
  | .ok term   => .println $ reduce term

def handleInput (input : String) : IO Unit := do
  if input.startsWith ":" then
    match parseCommand input with
    | .help none     => .println "Available commands\n :quit, :help"
    | .help (some s) => .println "Available commands\n :quit, :help" -- TODO: help function for commands
    | .quit          => .println "Bye" ; IO.Process.exit 0
    | .unknown ""
    | .unknown ":"   => .println "Unknown command"
    | .unknown cmd   => .println s!"Unknown command: {cmd.drop 1}"
  else
    evalExpression input

partial def replLoop : IO Unit := do
  IO.print "λ> "
  let stdin ← IO.getStdin
  let input := (← stdin.getLine).trimAscii.toString
  handleInput input
  replLoop
