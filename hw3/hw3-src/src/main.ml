open Ast

(* Your name *)
let my_name = "Matt Watson"


exception CannotStep
exception BopisnotValid


let parse (s:string) : expr =
let lexbuf = Lexing.from_string s in
let ast = Parser.prog Lexer.read lexbuf in ast


let rec isval (e:expr) : bool = match e with 
| Integer _ -> true
| Boolean _ -> true
| Fun _ -> true
| Nil -> true
| Cons(e1, e2) -> isval e1 && isval e2
| _ -> false


let calc bop e1 e2 = match bop, e1, e2 with
| Plus, Integer(v1), Integer(v2) ->  Integer(v1+v2)
| Minus, Integer(v1), Integer(v2) -> Integer(v1-v2)
| Times, Integer(v1), Integer(v2) -> Integer(v1*v2)
| Div, Integer(v1), Integer(v2) -> Integer(v1/v2)
| Leq, Integer(v1), Integer(v2) -> Boolean (v1<=v2)
| Geq, Integer(v1), Integer(v2) -> Boolean (v1>=v2)
| Le, Integer(v1), Integer(v2) -> Boolean (v1<v2)
| Ge, Integer(v1), Integer(v2) -> Boolean(v1>v2)
| Eq, Integer(v1), Integer(v2) -> Boolean(v1=v2)
| And, Boolean(v1), Boolean(v2) -> Boolean (v1 && v2)
| Or, Boolean(v1), Boolean(v2) -> Boolean (v1 || v2)
| _ -> raise BopisnotValid


let rec subst x (e1:expr) (e2:expr) = match e2 with 
| Var(z) -> if z=x then e1 else Var(z)
| Integer(w) -> Integer(w)
| Boolean(w) -> Boolean(w)
| Binop(bop, e3, e4) -> Binop (bop, (subst x e1 e3),(subst x e1 e4)) 
| Let(y, e3, e4) -> if y=x then Let(y, (subst x e1 e3), e4) else Let(y, (subst x e1 e3), (subst x e1 e4))
| If(e3, e4, e5) ->  If((subst x e1 e3), (subst x e1 e4), (subst x e1 e5))
| Fun(y, e) -> if y = x then Fun(y, e) else Fun(y, subst x e1 e)
| App(e3, e4) -> App(subst x e1 e3, subst x e1 e4)
| Nil -> Nil
| Cons(e3, e4) -> Cons(subst x e1 e3, subst x e1 e4)
| Match(e3, e4, y, z, e5) -> if y=x || z=x then Match(subst x e1 e3, subst x e1 e4, y, z, e5) else Match(subst x e1 e3, subst x e1 e4, y, z, subst x e1 e5)

let rec step (w:expr) : expr = match w with
| Var _ -> raise CannotStep
| Integer _ -> raise CannotStep
| Boolean _ -> raise CannotStep
| Binop (bop, e1, e2) -> if(isval e1) then (if (isval e2) then (calc bop e1 e2) else Binop(bop, e1, step e2)) else Binop(bop, step e1, e2)
| Let(x, e1, e2) -> if(isval e1) then (subst x e1 e2) else Let(x, step e1, e2)
| If(e, e1, e2) -> if(e=Boolean(true)) then e1 else (if(e=Boolean(false)) then e2 else If(step e, e1, e2))
| Fun _ -> raise CannotStep
| App (Fun (x, e1), e2) -> if (isval e2) then (subst x e2 e1) else (App(Fun(x, e1), step e2))
| App (e1, e2) -> if (isval e1) then (App(e1, step e2)) else (App(step e1, e2)) 
| Nil -> Nil
| Cons(e1, e2) -> if(isval e1) then (if(isval e2) then Cons(e1, e2) else Cons(e1, step e2)) else Cons(step e1, e2)
| Match(e1, e2, x, y, e3) -> if (isval e1) then match e1 with
       | Nil -> e2 
       | Cons(h,t) -> (subst y t (subst x h e3)) 
       | _ -> raise CannotStep
       else Match (step e1, e2, x, y, e3)

let rec stepstar (e:expr) : expr = 
if (isval e) then e else (stepstar (step e))

let interp_small (s:string) : expr = 
step (parse s)

let interp_small_eval (s:string) : expr = 
let e= parse s in stepstar e
