(* 
(* test_interp02.ml *)

open Lib
open Utils

(* Function to print expressions for debugging *)
let rec string_of_expr expr =
  match expr with
  | Unit -> "Unit"
  | True -> "True"
  | False -> "False"
  | Num n -> "Num " ^ string_of_int n
  | Var x -> "Var " ^ x
  | If (e1, e2, e3) ->
      "If (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | Bop (op, e1, e2) ->
      "Bop (" ^ string_of_bop op ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Fun (arg, ty, body) ->
      "Fun (" ^ arg ^ " : " ^ string_of_ty ty ^ ", " ^ string_of_expr body ^ ")"
  | App (e1, e2) ->
      "App (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Let { is_rec; name; ty; value; body } ->
      let rec_str = if is_rec then "rec " else "" in
      "Let (" ^ rec_str ^ name ^ " : " ^ string_of_ty ty ^ " = " ^ string_of_expr value ^ " in " ^ string_of_expr body ^ ")"
  | Assert e ->
      "Assert (" ^ string_of_expr e ^ ")"

(* Test cases *)
let test_parse_and_desugar input =
  match parse input with
  | None ->
      print_endline "Parse failed."
  | Some prog ->
      print_endline "Parse succeeded.";
      let expr = desugar prog in
      print_endline "Desugared expression:";
      print_endline (string_of_expr expr)

let () =
  let test_inputs = [
    (* Simple variable binding *)
    "let x : int = 42";
    (* Function definition *)
    "let f (x : int) (y : int) : int = x + y";
    (* Recursive function *)
    "let rec fact (n : int) : int = if n <= 1 then 1 else n * fact (n - 1)";
    (* Let-in expression *)
    "let x : int = 10 in x + 5";
    (* Nested let *)
    "let x : int = 1 in let y : int = 2 in x + y";
    (* Anonymous function *)
    "fun (x : int) (y : int) -> x + y";
    (* Application *)
    "f 3 4";
    (* If expression *)
    "if true then 1 else 0";
    (* Assert *)
    "assert (x > 0)";
    (* Complex expression *)
    "let x : int = 5 in let y : int = x * 2 in fun (z : int) -> x + y + z"
  ] in
  List.iter test_parse_and_desugar test_inputs *)
