open Ast
(* GENERAL UTILITIES *)

let rec find_class cname type_info = 
  match type_info with
  | t1 :: tRest -> 
      if (String.equal cname t1.name) 
        then (Some t1)
        else find_class cname tRest
  | [] -> None
  ;;

let rec find_method mid cType =
  find_method_data mid cType.method_data
and find_method_data mid mData = 
  match mData with
  | m1 :: mRest -> if (m1.method_name = mid) then Some m1 else (find_method_data mid mRest)
  | [] -> None
  ;;

let rec analyze_identifier id vList = 
  match vList with
  | v1 :: vRest -> if (v1.varname = id) then (false, v1.t) else analyze_identifier id vRest
  | [] -> print_string ("variable " ^ id ^ " has not been defined") ; (true, Int)
  ;;

let check_type expected actual generating_expr = 
  let error_found = not (expected = actual) in 
  if error_found then 
  (match expected with 
  | IntArr -> print_string "Expected an integer array from expression "
  | Bool -> print_string "Expected a boolean from expression "
  | Int -> print_string "Expected an integer from expression "
  | Id id -> print_string (("Expected a " ^ id.typename ^ " from expression " ^ (Pretty.print_expr generating_expr) ^ "\n"))) else ();
  error_found
  ;;

(* CHECKS FOR DUPLICATES *)

let rec contains_type id type_info =
  match type_info with
  | t1 :: tRest -> 
      (String.equal id t1.name) || (contains_type id tRest)
  | [] -> print_string ("type " ^ id ^ " has not been defined\n"); false

let rec has_duplicate_var var_list = 
  match var_list with
  | v1 :: vRest -> (is_duplicate_var v1.varname vRest) || (has_duplicate_var vRest)
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

(* END CHECKS FOR DUPLICATES *)


(* Check if the type is indeed defined *)
let analyze_type t type_info cType=
  match t with
  | Id id -> (not (contains_type id type_info), t)
  | _ -> (false, t)
  ;;

let rec analyze_list_expr eList vList type_info cType =
  match eList with
  | e1 :: eRest -> 
    let (error_found_e1, e1_type) = analyze_expr e1 vList type_info cType in
    let (error_found_eRest, eRest_type) = analyze_list_expr eRest vList type_info cType in 
    (error_found_e1 || error_found_eRest, e1_type :: eRest_type) 
  | [] -> (false, [])

and analyze_uop uop vList type_info cType = 
  let (error_found, t) = (analyze_expr uop.exp vList type_info cType) in 
  let error_found = (error_found || (t = Bool)) in 
  print_string ("error found for expression " ^ Pretty.print_expr uop.exp); 
  (error_found, Bool)

and analyze_bop bop vList type_info cType =
  let (error_found_lhs, tLeft) = analyze_expr bop.binop_lhs vList type_info cType in
  let (error_found_rhs, tRight) = analyze_expr bop.binop_rhs vList type_info cType in
  let exp = Binop bop in 
  let (error_found_type, ret_type) = match bop.op with
  | Add -> ((check_type Int tLeft exp) || (check_type Int tRight exp), Int)
  | Subtract -> ((check_type Int tLeft exp) || (check_type Int tRight exp), Int)
  | Multiply -> ((check_type Int tLeft exp) || (check_type Int tRight exp), Int)
  | And -> ((check_type Bool tLeft exp) || (check_type Bool tRight exp), Bool)
  | LT -> ((check_type Int tLeft exp) || (check_type Int tRight exp), Int)
  in (error_found_lhs || error_found_rhs || error_found_type, ret_type)
and analyze_array_lookup alup vList type_info cType = 
  let (error_found_array, tArray) = analyze_expr alup.alup_array vList type_info cType in 
  let (error_found_index, tIndex) = analyze_expr alup.alup_index vList type_info cType in 
  let error_found_array_type = if (tArray = IntArr) then false else true in
  let error_found_index_type = if (tIndex = Int) then false else true in
  (error_found_array || error_found_index || error_found_array_type || error_found_index_type, IntArr)

and analyze_object_function_call ofc vList type_info cType = 
  let (error_found_obj, object_type) = analyze_expr ofc.obj vList type_info cType in
  let (error_found_args, args_type) = analyze_list_expr ofc.args vList type_info cType in
  let m_info = find_method ofc.name cType in 
  match m_info with
  | Some m_info ->
    let error_num_args = not ((List.length m_info.method_args) = (List.length ofc.args)) in
    let rec compare_type_lists tListPair arg_num = 
      (match tListPair with
      | (tL1::tLRest, tR1::tRRest) -> 
          let error_found = check_type tL1.t tR1 (ObjectFunctionCall ofc) in
          error_found || (compare_type_lists (tLRest, tRRest) (arg_num + 1))
      | (tL1::tLRest, []) -> false
      | ([], tR1::tRRest) -> false
      | ([], []) -> false) in
    let error_found_method = error_num_args || (compare_type_lists (m_info.method_args, args_type) 0) in
    let error_found_return_type = check_type m_info.method_return_type object_type (ObjectFunctionCall ofc) in
    (error_found_obj || error_found_args || error_found_method || error_found_return_type, m_info.method_return_type)
  | None -> (true, Int)

and analyze_expr e1 vList type_info cType = 
  match e1 with
  | Unop uop -> analyze_uop uop vList type_info cType
  | Binop bop -> analyze_bop bop vList type_info cType
  | ArrayLookup alup -> analyze_array_lookup alup vList type_info cType
  | ArrayLength alen -> 
      let (error_found, aType) = analyze_expr alen.alen_array vList type_info cType in
      let error_found_type = (aType = IntArr) in
      (if error_found_type then print_string ("type of " ^ Pretty.print_expr (ArrayLength alen) ^ " is not an array type\n") else ());
      (error_found || error_found_type, Int)
  | ObjectFunctionCall ofc -> analyze_object_function_call ofc vList type_info cType 
  | Const c -> 
    (match c with
    | IntConst _ -> (false, Int)
    | BoolConst _ -> (false, Bool))
  | Id id -> analyze_identifier id vList
  | This -> (false, Id { typename = cType.name })
  | NewArray e1 ->  
      let (error_found, t) = analyze_expr e1 vList type_info cType in
      let error_found_type = not (t = Int) in
      (if error_found_type then (print_string ("type of " ^ (Pretty.print_expr e1) ^ " is not int")) else ());
      (error_found || error_found_type, Int)
  | NewObject cid -> 
      match find_class cid type_info with
      | Some custom_type -> (false, Id { typename = custom_type.name} )
      | None -> print_string ("class " ^ cid ^ " does not exist\n"); (true, Int)
  | _ -> assert false
  ;;

let rec analyze_list_statement sList vList type_info cType = 
  match sList with 
  | s1 :: sRest -> (analyze_statement s1 vList type_info cType) || (analyze_list_statement sRest vList type_info cType)
  | [] -> false
and analyze_statement s1 vList type_info cType = 
  match s1 with
  | Block sList -> analyze_list_statement sList vList type_info cType
  | IfElse if_else -> 
      let (error_found_cond, tcond) = analyze_expr if_else.condition vList type_info cType in
      let error_found_cond_type = not (tcond = Bool) in
      (if error_found_cond_type then (print_string ("type " ^ (Pretty.print_expr if_else.condition) ^ " is not of boolean type")) else ());
      let error_found_if = analyze_statement if_else.if_block vList type_info cType in
      let error_found_else = analyze_statement if_else.else_block vList type_info cType in
      error_found_cond || error_found_cond_type || error_found_if || error_found_else 
  | While while_block -> 
      let (error_found_cond, tcond) = analyze_expr while_block.condition vList type_info cType in
      let error_found_cond_type = not (tcond = Bool) in
      (if error_found_cond_type then (print_string ("type " ^ (Pretty.print_expr while_block.condition) ^ " is not of boolean type")) else ());
      let error_found_block = analyze_statement while_block.block vList type_info cType in
      error_found_cond || error_found_cond_type || error_found_block
  | Println e1 -> fst (analyze_expr e1 vList type_info cType)
  | Assign v_assign -> 
      let (error_found_lhs, lhs_type) = analyze_identifier v_assign.lhs vList in
      let (error_found_rhs, rhs_type) = analyze_expr v_assign.rhs vList type_info cType in
      let error_found_type = not (lhs_type = rhs_type) in
      if error_found_type then (print_string ("expression " ^ (Pretty.print_expr v_assign.rhs) ^ " does not match the expected type")) else ();
      error_found_lhs || error_found_rhs || error_found_type
  | ArrayAssign a_assign ->
      let (error_found_array, array_type) = analyze_identifier a_assign.array vList in
      let error_found_array_type = not (array_type = IntArr) in
      if error_found_array_type then print_string ("variable " ^ a_assign.array ^ " is not of integer array type") else ();
      let (error_found_index, index_type) = analyze_expr a_assign.index vList type_info cType in
      let (error_found_rhs, rhs_type) = analyze_expr a_assign.rhs vList type_info cType in
      let error_found_index_type = check_type Int index_type a_assign.index in
      let error_found_rhs_type = check_type Int rhs_type a_assign.rhs in
      error_found_array || error_found_array_type || error_found_index || error_found_rhs || error_found_index_type || error_found_rhs_type
  ;;

let rec analyze_list_methoddecl mList vList type_info cType =
  match mList with
  | m1 :: mRest -> (analyze_methoddecl m1 vList type_info cType) || (analyze_list_methoddecl mRest vList type_info cType)
  | [] -> false
and analyze_methoddecl mDecl vList type_info cType = 
  let error_found = has_duplicate_var mDecl.args in
  let vList = List.append mDecl.vars (List.append mDecl.args vList) in
  let error_found = error_found || has_duplicate_var mDecl.vars in
  let error_found = error_found || (analyze_list_statement mDecl.stmts vList type_info cType) in 
  let (error_tmp,t) = analyze_expr mDecl.returnexpr vList type_info cType in
  let error_found = error_found || error_tmp || (check_type mDecl.returntype t mDecl.returnexpr) in
  error_found
  ;;

let rec analyze_list_classdecl cList type_info =
  match cList with
  | c1 :: cRest -> (analyze_classdecl c1 type_info) || (analyze_list_classdecl cRest type_info)
  | [] -> false
and analyze_classdecl cDecl type_info = 
  let error_found = has_duplicate_var cDecl.vars in
  let cType = (find_class cDecl.classname type_info) in
  let cType = match cType with
  | Some c -> c
  | None -> assert false
  in let error_found = error_found || (analyze_list_methoddecl cDecl.methods cDecl.vars type_info cType) in
  error_found
  ;;

let analyze_mainclass mClass type_info = 
  analyze_statement mClass.stmt [] type_info { name = ""; var_data = []; method_data = []; superclass = None } 
  ;;

let analyze prog type_info =
  let error_found = has_duplicate_type type_info in
  let error_found = error_found || analyze_mainclass prog.mclass type_info in
  let error_found = error_found || analyze_list_classdecl prog.classes type_info in
  error_found
  ;;
