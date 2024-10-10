%{
open Ast
%}

(* token declarations *)
%token PLUS 
%token TIMES 
%token LEQ 
%token AND 
%token OR 
%token IF 
%token THEN 
%token ELSE
%token LET 
%token IN
%token EQ
%token LPAREN
%token RPAREN
%token TRUE
%token FALSE
%token <int> INT
%token <string> VAR
%token EOF 

(* associative and priority *)
%nonassoc IN
%nonassoc ELSE
%left PLUS
%left TIMES
%right OR
%left AND


%start <Ast.expr> prog
%%


(* grammar *)

prog: 
e=expr; EOF {e};

expr:
| x = VAR { Var (x) }
| i = INT { Integer (i) }
| TRUE { Boolean (true) }
| FALSE { Boolean (false) }
| e1= expr ;  PLUS; e2=expr {Binop( Plus, e1, e2) } 
| e1= expr ;  TIMES; e2=expr {Binop( Times, e1, e2) }
| e1= expr ; LEQ; e2=expr {Binop(Leq, e1, e2)}
| e1= expr; AND; e2=expr {Binop(And, e1,e2) }
| e1= expr; OR; e2=expr {Binop(Or, e1,e2) }
| LPAREN; e1=expr; RPAREN {e1}
| LET; x= VAR; EQ; e1=expr; IN; e2=expr {Let (x, e1,e2)}
| IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {If (e1,e2,e3)}