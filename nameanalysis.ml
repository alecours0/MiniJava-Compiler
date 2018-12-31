open Ast

let rec find_class cname type_info = 
  match type_info with
  | t1 :: tRest -> 
      if (String.equal cname t1.name) 
        then (Some t1)
        else find_class cname tRest
  | -> None
  ;;

let rec contains_method id type_info =
  
and  
  ;;

let rec contains_type id type_info =
  match type_info with
  | t1 :: tRest -> 
      (String.equal id t1.name) || (contains_type tRest type_info)
  | [] -> print_string ("type " ^ id " has not been defined\n"); false

let rec has_duplicate_var var_list = 
  match var_list with
  | v1 :: vRest -> (is_duplicate_var v1.varname vRest) || (has_duplicate_type vRest)
  | [] -> false
and is_duplicate_var varname var_list = 
  match var_list with
  | v1 :: vRest -> 
    if (String.equal varname v1.varname)
    then (print_string ("var " ^ varname ^ "is a duplicate name"); true)
    else is_duplicate_var varname vRest
  | [] -> false
  ;;

let rec has_duplicate_type type_info = 
  match type_info with
  | t1 :: tRest -> (is_duplicate_type t1.name tRest) || (has_duplicate_type tRest)
  | [] -> false
and is_duplicate_type typename type_info =
  match type_info with
  | t1 :: tRest -> 
    if (String.equal typename t1.name)
    then (print_string ("type " ^ typename ^ "is a duplicate type"); true)
    else is_duplicate_type typename tRest
  | [] -> false
  ;;

(* Check if the type is indeed defined *)
let analyze_type t type_info =
  match t with
  | Id id -> not (contains_type id type_info)
  | _ -> false
  ;;

let rec analyze_list_expr eList =
  match eList with
  | e1 :: eRest -> (analyze_expr e1) || (analyze_list_expr eRest)
  | [] -> false
and analyze_expr e1 = 
  match e1 with
  | Unop uop -> analyze_expr uop.exp
  | Binop bop -> (analyze_expr bop.lhs) || (analyze_expr bop.rhs)
  | ArrayLookup alup -> (analyze_expr alup.array) || (analyze_expr alup.index)
  | ArrayLength alen -> analyze_expr alen.array
  | ObjectFunctionCall ofc -> (analyze_expr ofc.obj) || (contains_method ofc.name) || (analyze_list_expr ofc.args)
  | Const c -> 
  | Id id -> id
  | This -> 
  | NewArray e -> 
  | NewObject n -> 
  ;;

let rec analyze_list_statement sList type_info = 
  match sList with 
  | s1 :: sRest -> 
  | [] -> false
and analyze_statement s = 
  match s with
  | Block sList -> 
  | IfElse if_else -> 
  | While while_block -> 
  | Println e -> analyze_expr e type_info
  | Assign v_assign -> 
  | ArrayAssign a_assign ->
  ;;

let rec analyze_list_methoddecl mList = 
  match mList with
  | m1 :: mRest -> (analyze_methoddecl m1 type_info) || 
  | [] -> false
and analyze_methoddecl mDecl = 
  ;;

let rec analyze_list_classdecl cList type_info =
  match cList with
  | c1 :: cRest -> (analyze_classdecl c1 type_info) || (analyze_list_classdecl cRest type_info)
  | [] -> false
and analyze_classdecl cDecl type_info = 
  let error_found = analyze_list_var cDecl.vars type_info in
  let error_found = error_found || (analyze_list_methoddecl cDecl.methods type_info)
  ;;

let analyze_mainclass mClass type_info = 
  analyze_statement mClass.stmt type_info
  ;;

let analyze prog type_info =
  let error_found = analyze_mainclass prog.mclass type_info in
  let error_found = error_found || analyze_classes prog.classes type_info
  ;;
