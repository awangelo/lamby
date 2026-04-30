import Lamby

def showUsage : IO Unit :=
  IO.print
s!"Usage:  lamby [options]

A simple untyped Lambda Calculus interpreter.

Options:
    -h, --help      Show this help message.

REPL Commands:
    :help, :h       Show available commands.
    :help <term>    Show definition of a standard term (e.g., :help omega).
    :quit, :q       Exit the interpreter.

Syntax:
    Abstraction     \\x. body  OR  λx. body
    Application     func arg
    Variable        x
    Grouping        (func arg)

Examples:
    λ> (\\x. x) y
    λ> (\\x. \\y. x) a b
"

def main : List String → IO Unit
  | [] => Lamby.replLoop
  | ["--help"] | ["-h"] => showUsage
  | _ => IO.println "Invalid arguments. Use --help for usage information."
