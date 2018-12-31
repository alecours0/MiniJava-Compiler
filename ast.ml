type typ =
  | IntArr
  | Bool
  | Int
  | Id of identifier_type
and identifier_type =
{
  typename : string
};;

type var =
{
  t : typ;
  varname : string;
};;

(* expr *)
type binary_operation =
  | Add
  | Subtract
  | Multiply
  | And
  | LT;;
  
type unary_operation =
  | Not;;

type expr =
  | Unop of unop
  | Binop of binop
  | ArrayLookup of array_lookup
  | ArrayLength of array_length
  | ObjectFunctionCall of object_function_call
  | Const of const
  | Id of string
  | This
  | NewArray of expr
  | NewObject of string
and unop =
{
  op : unary_operation;
  exp : expr
}
and binop =
{
  op : binary_operation;
  lhs : expr;
  rhs : expr
}
and array_lookup =
{
  array : expr;
  index : expr
}
and array_length =
{
  array : expr
}
and object_function_call =
{
  obj : expr;
  name : string;
  args : (expr list)
}
and const =
  | IntConst of int
  | BoolConst of bool;;

(* statement *)

type statement =
  | Block of (statement list)
  | IfElse of if_else_stmt
  | While of while_stmt
  | Println of expr 
  | Assign of assign_stmt
  | ArrayAssign of array_assign_stmt
and if_else_stmt =
{
  condition : expr;
  if_block : statement;
  else_block : statement
}
and while_stmt =
{
  condition : expr;
  block : statement
}
and assign_stmt =
{
  lhs : string;
  rhs : expr
}
and array_assign_stmt =
{
  array : string;
  index : expr;
  rhs : expr
};;

type methoddecl =
{
  returntype : typ;
  methodname : string;
  args : (var list);
  vars : (var list);
  stmts : (statement list);
  returnexpr : expr;
};;

type classdecl =
{
  classname : string;
  vars : (var list);
  methods : (methoddecl list);
  is_subclass : bool;
  superclass_name : string
};;

type mainclass =
{
  mclassname : string;
  stmt : statement;
};;

type program =
{
  mclass : mainclass;
  classes : (classdecl list);
};;

type method_info = 
{
  method_name : string;
  method_args : (var list);
  method_return_type : typ;
  method_locals : (var list)
}
and customtype = 
{
  name : string;
  var_data : (var list);
  method_data : (method_info list);
  superclass : string option
};;
