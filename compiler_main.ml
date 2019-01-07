let read_line i = try Some (input_line i) with End_of_file -> None 

let lines_from_files filename = 
  let rec lines_from_files_aux i acc = match (read_line i) with 
    | None -> List.rev acc
    | Some s -> lines_from_files_aux i (s :: acc) in 
  lines_from_files_aux (open_in filename) [] 

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let rec print_lines sList = 
  match sList with
  | s1 :: sRest -> (print_string s1); print_lines sRest 
  | [] -> ()

let () =
  let code_lines = read_file Sys.argv.(1) in
  let code = String.concat "" code_lines in
  let lexbuf = Lexing.from_string code in
  let prog = Minijavaparser.main Minijavalexer.token lexbuf in
  let type_info = Type_information.collect_type_info prog in
  let error_found = Typecheck.analyze prog type_info in
  if error_found then print_string "valid program" else print_string "invalid program"
  
  (* Pretty.print_ast prog *)
  
