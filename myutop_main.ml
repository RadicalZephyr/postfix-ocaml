(* Compile this with: *)
(* ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package utop
             -package llvm myutop_main.ml -cc g++ *)

(* Then you have a utop that has the llvm libs linked in *)

(* Then run it from within emacs like so:
   "opam config exec ./llvmutop -- -emacs" *)
let () = UTop_main.main ()
