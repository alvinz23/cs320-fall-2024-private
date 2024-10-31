/* lib/parser/lex.mll */

{
  open Par
}

(* Define regular expressions for different token patterns *)
let whitespace = [' ' '\t' '\n' '\r']+                             (* Whitespace *)
let num = '-'? ['0'-'9']+                                          (* Number: optional '-' followed by digits *)
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*      (* Variable: starts with lowercase letter or '_', followed by alphanumerics or '_' or '\'' *)

let word_if = "if"
let word_then = "then"
let word_else = "else"
let word_let = "let"
let word_in = "in"
let word_fun = "fun"
let word_true = "true"
let word_false = "false"

let op_or = "||"
let op_and = "&&"
let op_leq = "<="
let op_geq = ">="
let op_neq = "<>"
let op_arrow = "->"
let op_mod = "mod"
let op_plus = "+"
let op_minus = "-"
let op_times = "*"
let op_divide = "/"
let op_lt = "<"
let op_gt = ">"
let op_eq = "="
let lparen = "("
let rparen = ")"
let unit = "()"

rule read = parse
  | op_arrow { ARROW }
  | op_or { OR }
  | op_and { AND }
  | op_leq { LEQ }
  | op_geq { GEQ }
  | op_neq { NEQ }
  | op_mod { MOD }
  | word_if { IF }
  | word_then { THEN }
  | word_else { ELSE }
  | word_let { LET }
  | word_in { IN }
  | word_fun { FUN }
  | word_true { TRUE }
  | word_false { FALSE }
  | op_plus { PLUS }
  | op_minus { MINUS }
  | op_times { TIMES }
  | op_divide { DIVIDE }
  | op_lt { LT }
  | op_gt { GT }
  | op_eq { EQ }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | unit { UNIT }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
  | _ as c { failwith ("Unrecognized character: " ^ String.make 1 c) }
