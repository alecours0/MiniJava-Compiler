open Ast

let rec collect_list_method_info mList = 
  match mList with
  | m1 :: mRest -> (collect_method_info m1) :: (collect_list_method_info mRest)
  | [] -> []
and collect_method_info mDecl = 
  { method_name = mDecl.methodname;
    method_args = mDecl.args;
    method_return_type = mDecl.returntype;
    method_locals = mDecl.vars }
  ;;
  
let rec collect_list_class_info cList =
  match cList with
  | c1 :: cRest -> (collect_class_info c1) :: (collect_list_class_info cRest)
  | [] -> []
and collect_class_info cDecl = 
  let c_superclass = if cDecl.is_subclass then Some cDecl.superclass_name else None in
  let m_data = collect_list_method_info cDecl.methods in
  { name = cDecl.classname; 
    var_data = cDecl.vars; 
    method_data = m_data;
    superclass = c_superclass }
  ;;

let collect_type_info prog =
  collect_list_class_info prog.classes
  ;;
