open Ast

let print_type t =
  match t with
  | IntArr -> "int[]"
  | Bool -> "bool"
  | Int -> "int"
  | Id id -> id.typename
  ;;

let print_const c = 
  match c with
  | IntConst i -> string_of_int i
  | BoolConst b -> string_of_bool b
  ;;

let rec print_list_vardecl vList =
  match vList with
  | v1 :: vRest -> (print_var v1) ^ ";\n" ^ (print_list_vardecl vRest)
  | [] -> ""
and print_list_formal vList =
  match vList with
  | v1 :: vRest -> (print_var v1) ^ ", " ^ (print_list_formal vRest)
  | [] -> ""
and print_var t = 
  (print_type t.t) ^ " " ^ t.varname  
  ;;

let rec print_list_expr e =
  match e with
  | e1 :: eList -> (print_expr e1) ^ ", " ^ (print_list_expr eList)
  | [] -> ""
and print_expr t = 
  match t with
  | Unop uop -> (match uop.op with
                  | Not -> "!"
                  | _ -> assert false) ^
                print_expr uop.exp
  | Binop bop -> (print_expr bop.lhs) ^ " " ^  
                 (match bop.op with
                       | Add -> "+"
                       | Subtract -> "-"
                       | Multiply -> "*"
                       | And -> "&&"
                       | LT -> "<"
                       | _ -> assert false) ^
                  " " ^ (print_expr bop.rhs)
  | ArrayLookup alup -> (print_expr alup.array) ^ "[" ^ (print_expr alup.index) ^ "]"
  | ArrayLength alen -> print_expr alen.array
  | ObjectFunctionCall ofc -> (print_expr ofc.obj) ^ "." ^ ofc.name ^ 
      "(" ^ (print_list_expr ofc.args) ^ ")"
  | Const c -> print_const c
  | Id id -> id
  | This -> "this"
  | NewArray e -> "new int[" ^ (print_expr e) ^ "]"
  | NewObject n -> "new " ^ n ^ "()"
  ;;

let rec print_list_statement s = 
  match s with 
  | s1 :: sList -> (print_statement s1) ^ (print_list_statement sList)
  | [] -> ""
and print_statement s = 
  match s with
  | Block sList -> "\n{\n" ^ (print_list_statement sList) ^ "\n}\n"
  | IfElse if_else -> "if(" ^ (print_expr if_else.condition) ^ 
      ")\n" ^ (print_statement if_else.if_block) ^ "\nelse\n" ^
      (print_statement if_else.else_block)
  | While while_block -> "while(" ^ (print_expr while_block.condition) ^
      ")\n" ^ (print_statement while_block.block)
  | Println e -> "System.out.println(" ^ (print_expr e) ^ ");\n"
  | Assign v_assign -> v_assign.lhs ^ " = " ^ (print_expr v_assign.rhs) ^ ";\n"
  | ArrayAssign a_assign -> a_assign.array ^ "[" ^ (print_expr a_assign.index) ^
      "] = " ^ (print_expr a_assign.rhs) ^ ";\n"
  ;;

let rec print_list_methoddecl mList = 
  match mList with
  | m1 :: mRest -> (print_methoddecl m1) ^ "\n" ^ (print_list_methoddecl mRest)
  | [] -> ""  
and print_methoddecl mDecl = 
  "public " ^ (print_type mDecl.returntype) ^ " " ^ mDecl.methodname ^ "(" ^ 
  (print_list_formal mDecl.args) ^ ")\n{\n" ^
  (print_list_vardecl mDecl.vars) ^ (print_list_statement mDecl.stmts) ^
  "return " ^ (print_expr mDecl.returnexpr) ^ ";\n}\n"
  ;;

let rec print_list_classdecl cList =
  match cList with
  | c1 :: cRest -> (print_classdecl c1) ^ "\n" ^ (print_list_classdecl cRest)
  | [] -> ""
and print_classdecl cDecl = 
  let superclass = 
    if cDecl.is_subclass 
      then (cDecl.superclass_name ^ " extends") 
      else "" in
  "class " ^ cDecl.classname ^ superclass ^
  "\n{\n" ^ (print_list_vardecl cDecl.vars) ^
  (print_list_methoddecl cDecl.methods) ^ "\n}\n"
  ;;

let print_mainclass mClass =
  "class " ^ mClass.mclassname ^
  "\n{\n{\npublic static void main(String[] args)\n{\n" ^
  (print_statement mClass.stmt) ^ "}\n}\n"
  ;;

let print_ast prog =
  let str = (print_mainclass prog.mclass) ^ 
  (print_list_classdecl prog.classes) in
  print_string str
  ;;
