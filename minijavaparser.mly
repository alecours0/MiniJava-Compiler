%{

open Ast

%}

(* Terminals  *)
%token          IF ELSE WHILE NEW THIS
%token          CLASS PUBLIC STATIC VOID
%token          MAIN LENGTH STRINGDEC INTDEC
%token          BOOLDEC TRUE FALSE PRINTLN
%token          DOT COMMA, SEMI, OPBRACE, CLBRACE;
%token          LBRACK RBRACK LPAREN
%token          RPAREN LT AND NOT ASSIGN PLUS
%token          TIMES MINUS RETURN EXTENDS
%token <string> ID
%token <int>    INTEGER

/* Precedences */
%left LT;
%left AND;
%left PLUS, MINUS;
%left TIMES;
%left NOT;
%left DOT;

/* Nonterminals */
%type <mainclass> MainClass
%type <classdecl> ClassDecl
%type <classdecl list> ClassDeclPrime
%type <var> VarDecl
%type <var list> VarDeclPrime
%type <methoddecl> MethodDecl
%type <methoddecl list> MethodDeclPrime
%type <var> FormalRest
%type <var list> FormalList FormalRestPrime
%type <statement> Statement
%type <statement list> StatementPrime
%type <expr> Exp ExpRest
%type <expr list> ExpRestPrime ExpList
%type <typ> Type

%type <program> program
%token EOF

%start <Ast.program> main
%%

/* Grammar */
main:
  | p = program; EOF
      { p }
;

program:
  | m1 = MainClass; cList = ClassDeclPrime
      { Ast.{ mclass = m1; classes = cList } }
;

MainClass:
    CLASS; id1 = ID; OPBRACE PUBLIC STATIC VOID MAIN LPAREN STRINGDEC LBRACK RBRACK; id2 = ID; RPAREN OPBRACE; s1 = Statement; CLBRACE CLBRACE
      { Ast.{ mclassname = id1; stmt = s1 } }
;

ClassDeclPrime:
  | c1 = ClassDecl; cList = ClassDeclPrime 
      { c1 :: cList }
  | 
      { [] }
;

ClassDecl:
    CLASS; id = ID; OPBRACE; vList = VarDeclPrime; mList = MethodDeclPrime; CLBRACE
    { let vList = List.rev vList in 
      Ast.{ classname = id; vars = vList; methods = mList; is_subclass = false; superclass_name = "" } }
  | CLASS; id1 = ID; EXTENDS; id2 = ID; OPBRACE; vList = VarDeclPrime; mList = MethodDeclPrime; CLBRACE
    { Ast.{ classname = id1; vars = vList; methods = mList; is_subclass = true; superclass_name = id2 } }
;

VarDeclPrime:
    vList = VarDeclPrime; v1 = VarDecl /* For some reason, making this left-recursive fixes the ASSIGN bug */
      { v1 :: vList }
  | 
      { [] }
;

VarDecl:
    t1 = Type; id = ID; SEMI 
    { Ast.{ t = t1; varname = id } }
;

MethodDeclPrime: 
    m1 = MethodDecl; mList = MethodDeclPrime  
    { m1 :: mList }
  | 
    { [] }
;

MethodDecl: 
    PUBLIC; t1 = Type; id = ID; LPAREN; fList = FormalList; RPAREN OPBRACE; vList = VarDeclPrime; sList = StatementPrime; RETURN; e1 = Exp; SEMI CLBRACE
    { let vList = List.rev vList in
      Ast.{ returntype = t1; methodname = id; args = fList; vars = vList; stmts = sList; returnexpr = e1 } }
;

FormalList:
    t1 = Type; id1 = ID; fList = FormalRestPrime
    { let f1 = Ast.{ t = t1; varname = id1 } in f1 :: fList }
  | 
    { [] }
;

FormalRestPrime:
    f1 = FormalRest; fList = FormalRestPrime
    { f1 :: fList }
  | 
    { [] }
;

FormalRest:
    COMMA; t1 = Type; id = ID
    { Ast.{ t = t1; varname = id } }
;

Type: 
    INTDEC LBRACK RBRACK
    { Ast.IntArr }
  | BOOLDEC
    { Ast.Bool }
  | INTDEC
    { Ast.Int }
  | id = ID
    { Ast.Id { typename = id } }
;

StatementPrime:
    s1 = Statement; sList = StatementPrime 
    { s1 :: sList }
  | 
    { [] }
;

Statement:
  | OPBRACE; sList = StatementPrime; CLBRACE
    { Ast.Block sList }
  | IF LPAREN; e1 = Exp; RPAREN; s1 = Statement; ELSE s2 = Statement
    { Ast.IfElse { condition = e1; if_block = s1; else_block = s2 } }
  | WHILE LPAREN; e1 = Exp; RPAREN; s1 = Statement
    { Ast.While { condition = e1; block = s1 } }
  | PRINTLN LPAREN; e1 = Exp; RPAREN SEMI
    { Ast.Println e1 }
  | id = ID; ASSIGN; e1 = Exp; SEMI
    { Ast.Assign { lhs = id; rhs = e1 } }
  | id = ID; LBRACK; e1 = Exp; RBRACK ASSIGN; e2 = Exp; SEMI
    { Ast.ArrayAssign { array = id; index = e1; rhs = e2 } }
;

Exp:
    e1 = Exp; AND; e2 = Exp
    { Ast.Binop { lhs = e1; rhs = e2; op = Ast.And } }
  | e1 = Exp; LT; e2 = Exp
    { Ast.Binop { lhs = e1; rhs = e2; op = Ast.LT } }
  | e1 = Exp; PLUS; e2 = Exp 
    { Ast.Binop { lhs = e1; rhs = e2; op = Ast.Add } }
  | e1 = Exp; MINUS; e2 = Exp
    { Ast.Binop { lhs = e1; rhs = e2; op = Ast.Subtract } }
  | e1 = Exp; TIMES; e2 = Exp
    { Ast.Binop { lhs = e1; rhs = e2; op = Ast.Multiply } }
  | e1 = Exp; LBRACK; e2 = Exp; RBRACK
    { Ast.ArrayLookup { array = e1; index = e2 } }
  | e1 = Exp; DOT LENGTH
    { Ast.ArrayLength { array = e1} }
  | e1 = Exp; DOT; id = ID; LPAREN; eList = ExpList; RPAREN
    {  Ast.ObjectFunctionCall { obj = e1; name = id; args = eList } }
  | v = INTEGER
    { Ast.Const (Ast.IntConst v) }
  | TRUE
    { Ast.Const (Ast.BoolConst true) }
  | FALSE
    { Ast.Const (Ast.BoolConst false) }
  | id = ID
    { Ast.Id id }
  | THIS
    { Ast.This }
  | NEW INTDEC LBRACK; e1 = Exp; RBRACK
    { Ast.NewArray e1 }
  | NEW; id = ID; LPAREN RPAREN
    { Ast.NewObject id }
  | NOT; e1 = Exp
    { Ast.Unop { op = Ast.Not; exp = e1 } }
  | LPAREN; e1 = Exp; RPAREN
    { e1 }
;

ExpList: 
    e1 = Exp; eList = ExpRestPrime 
    { e1 :: eList }
  | 
    { [] }
;

ExpRestPrime:
    e1 = ExpRest; eList = ExpRestPrime
    { e1 :: eList }
  | 
    { [] }
;

ExpRest:
    COMMA; e1 = Exp 
    { e1; }
;

%%