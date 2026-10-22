open Ast

let op_to_string op =
  match op with
  | Mult -> "*"
  | Div -> "/"
  | Add -> "+"
  | Sub -> "-"
  | Eq -> "=="
  | Neq -> "<>"
  | Gt -> ">"
  | Geq -> ">="
  | Lt -> "<"
  | Leq -> "<="

let priority (op : Ast.bop) : int =
  match op with
  | Mult | Div -> 3
  | Add | Sub -> 2
  | Eq | Neq | Gt | Geq | Lt | Leq -> 1

let rec pretty_print_expr (e : Ast.expr) : string =
  match e with
  | Int i -> string_of_int i 
  | Bool b -> string_of_bool b
  | Str s -> Printf.sprintf "\"%s\"" s
  | Var v -> v
  | Binop (op, l, r) ->
      let l_str = pretty_print_expr l in
      let r_str = pretty_print_expr r in
      let wrap e s =
        match e with
        | Binop (op', _, _) when priority op' < priority op -> "(" ^ s ^ ")"
        | _ -> s
      in
      Printf.sprintf "%s %s %s" (wrap l l_str) (op_to_string op) (wrap r r_str)

let rec print_stmt (tab : string) (s : stmt) : string =
	match s with
	| Assign (x, e) -> tab ^ x ^ " = " ^ pretty_print_expr e ^ ";\n"
	| Print e -> tab ^ "print(" ^ pretty_print_expr e ^ ");\n"
	| Skip -> tab ^ "skip;\n"
	| Cmp (s1, s2) -> print_stmt tab s1 ^ print_stmt tab s2

	| While (p, b) -> tab ^ "while (" ^ pretty_print_expr p ^ ")\n" ^ 
      tab ^ "{\n" ^ print_stmt (tab ^ "   ") b ^ tab ^ "}\n"

	| If(p, t, Skip) -> tab ^ "if (" ^ pretty_print_expr p ^ ") " ^ 
      "{\n" ^ print_stmt (tab ^ "   ") t ^ tab ^ "}\n"

	| If (p, t, e) -> tab ^ "if (" ^ pretty_print_expr p ^ ") " ^ 
      "{\n" ^ print_stmt (tab ^ "   ") t ^ tab ^ "}\n" ^ tab ^ 
      "else " ^ "{\n" ^ print_stmt (tab ^ "   ") e ^ tab ^ "}\n"
  | Halt -> tab ^ "Halt\n"
		
let pretty_print (s : stmt) : string = "{\n" ^ print_stmt "   " s ^ "}\n"

