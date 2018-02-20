(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let fromInt = function
    | 0 -> false
    | _ -> true

let toInt = function
    | false -> 0
    | true -> 1

let rec eval s e =
  match e with
    | Const (x) -> x
    | Var (name) -> s name
    | Binop (op, l, r) -> match op with
      | "!!" -> toInt (fromInt (eval s l) || fromInt (eval s r))
      | "&&" -> toInt (fromInt (eval s l) && fromInt (eval s r))
      | "==" -> toInt (eval s l == eval s r)
      | "!=" -> toInt (eval s l != eval s r)
      | "<=" -> toInt (eval s l <= eval s r)
      | "<"  -> toInt (eval s l <  eval s r)
      | ">=" -> toInt (eval s l >= eval s r)
      | ">"  -> toInt (eval s l >  eval s r)
      | "+"  -> eval s l  +  eval s r
      | "-"  -> eval s l  -  eval s r
      | "*"  -> eval s l  *  eval s r
      | "/"  -> eval s l  /  eval s r
      | "%"  -> eval s l mod eval s r
      | _    -> failwith "eval: unknown operation"
                    
