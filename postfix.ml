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
          Some (numargs, (List.t_of_sexp Ast.t_of_sexp (Sexp.List program)))
     end

| Sexp.List (_ :: _ :: _) ->
   fprintf stderr "Not a valid postfix program.\n\n";
   print_usage Sys.argv.(0);
   None

let compile program =
  match validate program with
  | None -> ()
  | Some (numargs, ast) ->
     ()




let () =
  match (Array.to_list Sys.argv) with
  (* If we don't get the right number of arguments, print out the usage
  message. *)
  | [] ->
     print_usage ""
  | pname :: [] ->
     print_usage pname
  | _ :: program :: [] ->
     compile program
  | pname :: _ :: _ ->
     print_usage pname
