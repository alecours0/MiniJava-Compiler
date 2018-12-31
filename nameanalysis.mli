(* 
 * Checks for the following name issues:
 * Use of an undefined symbol
 * Redeclaration of a symbol
 *)
val analyze : Ast.program -> Ast.(customtype list) -> bool