namespace Lamby

inductive Command where
  | eval (t : String)
  | help (target : Option String)  -- :help | :help <term> | :h
  | quit                           -- :quit | :q
  | unknown (cmd : String)

def parseCommand (input : String) : Command :=
  let input := input.trim

  if !input.startsWith ":" then
    Command.eval input
  else
    let parts := input.splitOn " " |>.filter (· != "")

    match parts with
    | [":quit"] | [":q"]           => Command.quit
    | [":help"] | [":h"]           => Command.help none
    | [":help", arg] | [":h", arg] => Command.help (some arg)
    | cmd :: _                     => Command.unknown cmd
    | []                           => Command.unknown ""



def loop : IO Unit := IO.println "REPL"
