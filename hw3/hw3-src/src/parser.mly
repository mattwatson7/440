%{
open Ast
%}

(* token declarations *)
%token PLUS 
%token MINUS
%token TIMES
%token DIV
%token LEQ
%token GEQ
%token LE
%token GE
%token EQ
%token AND 
%token OR 
%token IF 
%token THEN 
%token ELSE
%token LET 
%token IN
%token LPAREN
%token RPAREN
%token TRUE
%token FALSE
%token ORR
%token NIL
%token CONS
%token ARROW
%token MATCH
%token WITH
%token FUN
%token <int> INT
%token <string> VAR
%token EOF 

(* associative and priority *)
%nonassoc IN
%nonassoc ELSE
%left PLUS MINUS
%left TIMES DIV
%left LEQ GEQ LE GE EQ
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
| e1= expr ; MINUS; e2=expr {Binop( Minus, e1, e2) }
| e1= expr ;  TIMES; e2=expr {Binop( Times, e1, e2) }
| e1= expr ; DIV; e2=expr {Binop( Div, e1, e2) }
| e1= expr ; LEQ; e2=expr {Binop( Leq, e1, e2)}
| e1= expr; GEQ; e2=expr {Binop( Geq, e1, e2) }
| e1= expr; LE; e2=expr {Binop( Le, e1, e2) }
| e1= expr; GE; e2=expr {Binop( Ge, e1, e2) }
| e1= expr; EQ; e2=expr {Binop( Eq, e1, e2) }
| e1= expr; AND; e2=expr {Binop(And, e1,e2) }
| e1= expr; OR; e2=expr {Binop(Or, e1,e2) }
| LPAREN; e1=expr; RPAREN {e1}
| LET; x= VAR; EQ; e1=expr; IN; e2=expr {Let (x, e1,e2)}
| IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {If (e1,e2,e3)}
| e1 = expr; e2 = expr {App (e1,e2)}
| MATCH; e1 = expr; WITH; NIL; ARROW; e2 = expr; ORR; s1 = VAR; CONS; s2 = VAR; ARROW; e3 = expr {Match (e1, e2, s1, s2, e3)}
| e1 = expr; CONS; e2 = expr {Cons (e1, e2)}
| NIL; {Nil}
| v1 = VAR; ARROW; e1 = expr {FUN (v1, e1)}
