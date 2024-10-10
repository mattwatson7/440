open OUnit2
open Interp
open Ast
open Main

let make_t n i s =
  [n >:: (fun _ -> assert_equal (Integer i) (interp_small_eval s))]


let tests = [
  make_t "int" 22 "22";
  make_t "add" 22 "11+11";
  make_t "adds" 22 "(10+1)+(5+6)";
  make_t "let" 22 "let x=22 in x";
  make_t "lets" 22 "let x = 0 in let x = 22 in x";
  make_t "mul1" 22 "2*11";
  make_t "mul2" 22 "2+2*10";
  make_t "mul3" 14 "2*2+10";
  make_t "mul4" 40 "2*2*10";
  make_t "if1" 22 "if true then 22 else 0";
  make_t "if2" 22 "if (1+2) <= (3+4) then 22 else 0";
  make_t "if3" 22 "if (1+2) <= (3*4) then let x = 22 in x else 0";
  make_t "letif" 22 "let x = ((1+2) <= (3*4)) in if x then 22 else 0";
  make_t "funapp" 107 "(fun x -> (x+7)) 100";
  make_t "letfun" 36 "let x= (fun x -> (x+7)) in let z = (440/15) in x z";
  make_t "funfun" 102 "((fun x -> fun y -> x y) (fun x-> x+2)) 100";
  make_t "lethd1" 3 "let x= 3::2::1::[] in match x with []->-1 | hd::tl->hd";
  make_t "lethd2" (-1) "let x= [] in match x with []-> -1 | hd::tl->hd" ;
  make_t "true" 1 "if true then 1 else 0";
  make_t "leq" 1 "if 1<=1 then 1 else 0";
]

let _ = run_test_tt_main ("suite" >::: List.flatten tests)
