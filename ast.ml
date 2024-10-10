(** type of expressions *)

type bop = Plus | Times | Leq | And | Or | Minus | Div | Geq | Le | Ge | Eq 

type expr = Var of string | Integer of int | Boolean of bool | Binop of ( bop * expr * expr)|  If of (expr * expr * expr) | Let of ( string * expr * expr) |  Fun of ( string * expr) | App of (expr * expr) | Nil | Cons of (expr * expr) | Match of (expr * expr * string * string * expr)