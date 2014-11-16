open Core.Std

let print_usage pname =
  fprintf stderr "usage: %s <program>\n\n" pname;
  fprintf stderr "A valid postfix program consists of: ";
  fprintf stderr "(postfix <numargs> <commands>...)\n\n";
  fprintf stderr "numargs must be an integer indicating the number ";
  fprintf stderr "of arguments\nthat the program will take.\n"


let validate program =
  match Sexp.of_string program with
  | Sexp.Atom _
  | Sexp.List ([])
  | Sexp.List (_ :: [])
  | Sexp.List (_ :: _ :: []) ->
     fprintf stderr "Not a valid postfix program.\n\n";
     print_usage Sys.argv.(0);
     None

  | Sexp.List ((Atom "postfix") :: (Atom args) :: program) ->
     begin
       match Or_error.try_with (fun () -> Int.of_string args) with

       | Error _ ->
          fprintf stderr "%s is not an integer.\n\n" args;
          print_usage Sys.argv.(0);
          None

       | Ok numargs ->
          Some (numargs, (List.t_of_sexp Ast.parse_from_sexp (Sexp.List program)))
     end

| Sexp.List (_ :: _ :: _) ->
   fprintf stderr "Not a valid postfix program.\n\n";
   print_usage Sys.argv.(0);
   None

let compile program args =
  let (numargs, ast) = program in
  Interpreter.run numargs args ast

let postfix_program =
  Command.Spec.Arg_type.create
    begin
      fun program ->
      match validate program with
      | Some ret ->
         ret

      | None ->
         exit 1
    end

let spec =
  let open Command.Spec in
  empty
  +> anon ("program" %: postfix_program)
  +> anon (maybe_with_default [] (sequence ("numbers" %: int)))

let command =
  Command.basic
    ~summary:"Compile or interpret a postfix program"
    ~readme:(fun () -> "A valid postfix program consists of:\n(postfix <numargs>"
                       ^ " <commands>...)"
                       ^ "\nnumargs must be an integer indicating the number of"
                       ^ " arguments that the program will take.")
    spec
    (fun program arglist () -> compile program arglist)

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
