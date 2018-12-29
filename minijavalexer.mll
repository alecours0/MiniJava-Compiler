(* File minijavalexer.mll *)
{
open Minijavaparser        (* The type token is defined in parser.mli *)
}

(* regular definitions *)
let nl =                  "\n" | "\r\n" | "\r"
let not_nl =              [^ '\n' '\r']
let delim =               ' ' | '\t' | nl 
let ws =                  (delim)+
let letter =	            ['A' - 'Z'] | ['a' - 'z'] 
let digit =               ['0' - '9']
let identifier =          (letter) ((letter) | (digit) | '_')* 
let integer =             ("0x")? (digit)+ ('.' (digit)+)? ('E' ['+''-']? (digit)+)?
let linecommentnl =       "//" (not_nl)* ((nl)?)
(* let linecomment =         "//" (not_nl)* *)
(* let blockcomment =        ("/*" [\*]* "*/") | ("/*" ([^\/\*] | ([\*]+ [^\/\*])) ( ([^\*]) | ([\*]+ [^\/\*]) )* "*/") *)

rule token = parse           
| "."                     { DOT }
| ","                     { COMMA }
| ";"                     { SEMI }
| "{"                     { OPBRACE }
| "}"                     { CLBRACE }
| "["                     { LBRACK }
| "]"	                  { RBRACK }
| "("                     { LPAREN }
| ")"                     { RPAREN }
| "<"                     { LT }
| "&&"                    { AND }
| "!"                     { NOT }
| "="                     { ASSIGN }
| "+"                     { PLUS }
| "*"	                  { TIMES }
| "-"	                  { MINUS }
| "class"                 { CLASS }
| "public"                { PUBLIC }
| "static"                { STATIC }
| "void"                  { VOID }
| "main"                  { MAIN }
| "String"                { STRINGDEC }
| "extends"               { EXTENDS }
| "return"                { RETURN }
| "int"                   { INTDEC }
| "boolean"               { BOOLDEC }
| "if"                    { IF }
| "else"                  { ELSE }
| "while"                 { WHILE }
| "System.out.println"    { PRINTLN }
| "length"                { LENGTH }
| "true"                  { TRUE }
| "false"                 { FALSE }
| "this"                  { THIS }
| "new"                   { NEW }
| ws			  { token lexbuf }
| linecommentnl           { print_string (Lexing.lexeme lexbuf); token lexbuf }
| integer                 { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
| identifier              { ID (Lexing.lexeme lexbuf) }
| eof { EOF }


(* If add back blockcomment support, need to add it back 
   | eof          { EOF } 
   | linecomment  { print_string (Lexing.lexeme lexbuf); token lexbuf } *)