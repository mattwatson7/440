open OUnit2
open Interp
open Ast
open Main

let make_t n i s =
  [n >:: (fun _ -> assert_equal (Integer i) (typ_eval s))]

let make_e n i s =
  [n >:: (fun _ -> assert_raises i (fun () -> typ_eval s))]

let tests = [
  make_t "int" 22 "22";
  make_t "add" 22 "11+11";
  make_t "adds" 22 "(10+1)+(5+6)";
  make_t "let" 22 "let x:int=22 in x";
  make_t "lets" 22 "let x:int = 0 in let x:int = 22 in x";
  make_t "mul1" 22 "2*11";
  make_t "mul2" 22 "2+2*10";
  make_t "mul3" 14 "2*2+10";
  make_t "mul4" 40 "2*2*10";
  make_t "div1" 2 "2*10/10";
  make_t "div2" 100 "20/2*10";
  make_t "Geq" 0 "if 2>=10 then 1 else 0";
  make_t "Ge" 0 "if 2>10 then 1 else 0";
  make_t "Le" 1 "if 2<10 then 1 else 0";
  make_t "Eq" 0 "if 2=10 then 1 else 0";
  make_t "And" 0 "if true && false then 1 else 0";
  make_t "Or" 1 "if true || false then 1 else 0";
  make_t "if1" 22 "if true then 22 else 0";
  make_t "if2" 22 "if (1+2) <= (3+4) then 22 else 0";
  make_t "if3" 22 "if (1+2) <= (3*4) then let x:int = 22 in x else 0";
  make_t "letif" 22 "let x:bool = ((1+2) <= (3*4)) in if x then 22 else 0";
  make_t "funapp" 107 "(fun x :(int) -> (x+7)) 100";
  make_t "letfun" 462 "let x:(int -> int) = (fun x :(int) -> (x+7)) in (let z:(int) = (440+15) in x z)";
  make_t "funfun" 102 "((fun x :(int -> int) -> fun y:(int) -> x y) (fun x :(int) -> x+2)) 100";
  make_t "lethd1" 3 "let x: int list = 3::2::1::([]:(int list)) in match x:(int list) with []->-1 | hd::tl->hd";
  make_t "lethd2" (-1) "let x:int list= []:(int list) in match x:(int list)  with []-> -1 | hd::tl->hd" ;
  make_t "lethd3" 1 "if (let x:bool list= (true::[]:(bool list)) in match x:(bool list)  with []-> true | hd::tl->hd) then 1 else 0";
  make_t "true-e" 1 "if true then 1 else 0";
  make_e "if1-e" Interp.Main.Nottyped "if 1<=1 then 1 else true";
  make_e "if2-e" Interp.Main.Nottyped "if true then true else 0";
  make_e "if3-e" Interp.Main.Nottyped "if (1+2) <= (3+4) then 22 else 0+false";
  make_e "int-e" Interp.Main.Nottyped "22+false";
  make_e "add-e" Interp.Main.Nottyped "11*true";
  make_e "lethd1-e" Interp.Main.Nottyped "let x:bool list= []:(int list) in match x:(int list)  with []-> -1 | hd::tl->hd" ;
  make_e "lethd2-e" Interp.Main.Nottyped "let x:int list= []:(bool list) in match x:(int list)  with []-> -1 | hd::tl->hd" ;
  make_e "lethd3-e" Interp.Main.Nottyped "let x:int list= []:(int list) in match x:(int list)  with []-> true | hd::tl->hd" ;
  make_e "lethd4-e" Interp.Main.Nottyped "let x:int list= []:(bool list) in match x:(int list)  with []-> -1 | hd::tl->hd" ;
  make_e "lethd5-e" Interp.Main.Nottyped "let x:int list= []:(int list) in match x:(int list)  with []-> false | hd::tl->hd" ;
  make_e "lethd6-e" Interp.Main.Nottyped "let x:int list= []:(int list) in match x:(int list)  with []-> 0 | hd::tl->tl" ;
  make_e "lethd7-e" Interp.Main.Nottyped "let x:int list= (true::[]:(int list)) in match x:(int list)  with []-> 0 | hd::tl->hd" ;
  make_e "lethd8-e" Interp.Main.Nottyped "let x:bool list= (true::[]:(bool list)) in match x:(bool list)  with []-> 0 | hd::tl->hd" ;
  make_e "lethd9-e" Interp.Main.Nottyped "let x:int list= (true::[]:(bool list)) in match x:(bool list)  with []-> true | hd::tl->hd" ;
  make_e "funapp1-e" Interp.Main.Nottyped "(fun x :(bool) -> (x+7)) 100";
  make_e "funapp2-e" Interp.Main.Nottyped "(fun x :(bool) -> (x<=7)) true";
  make_e "letfun-e" Interp.Main.Nottyped "let x:(bool -> int) = (fun x :(int) -> (x+7)) in (let z:(int) = (440+15) in x z)";
  make_e "funfun-e" Interp.Main.Nottyped "((fun x :(int -> int) -> fun y:(int) -> x y) (fun x :(bool) -> x+2)) 100";
  make_e "mul1-e" Interp.Main.Nottyped "true*11";
  make_e "mul2-e" Interp.Main.Nottyped "2+2*true";
  make_e "mul3-e" Interp.Main.Nottyped "2*2/false";
  make_e "mul4-e" Interp.Main.Nottyped "false/3*10";
]

let _ = run_test_tt_main ("suite" >::: List.flatten tests)
