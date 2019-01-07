rm -rf *.cmo
rm -rf *.cmi

ocamllex minijavalexer.mll
menhir minijavaparser.mly

ocamlc -c ast.mli
ocamlc -c ast.ml

ocamlc -c pretty.mli
ocamlc -c pretty.ml

ocamlc -c minijavaparser.mli
ocamlc -c minijavalexer.ml
ocamlc -c minijavaparser.ml

ocamlc -c type_information.mli
ocamlc -c type_information.ml

ocamlc -c typecheck.mli
ocamlc -c typecheck.ml

ocamlc -c compiler_main.ml

ocamlc -o compiler type_information.cmo pretty.cmo minijavalexer.cmo minijavaparser.cmo typecheck.cmo compiler_main.cmo

echo "build successfully completed"
