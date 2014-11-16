open Core.Std

let print_usage pname =
  fprintf stderr "usage: %s <program>\n\n" pname;
  fprintf stderr "A valid postfix program consists of: ";
  fprintf stderr "(postfix <numargs> <commands>...)\n\n";
  fprintf stderr "numargs must be an integer indicating the number ";
  fprintf stderr "of arguments\nthat the program will take.\n"


let compile program =
  match Sexp.of_string program with
  | Sexp.Atom _
  | Sexp.List ([])
  | Sexp.List (_ :: [])
  | Sexp.List (_ :: _ :: []) ->
     fprintf stderr "Not a valid postfix program.\n\n";
     print_usage Sys.argv.(0)

  | Sexp.List ((Atom "postfix") :: (Atom args) :: program) ->
     begin
       match Or_error.try_with (fun () -> Int.of_string args) with

       | Error _ ->
          fprintf stderr "%s is not an integer.\n\n" args;
          print_usage Sys.argv.(0)

       | Ok numargs ->
          printf "Valid postfix program of %d args: '%s'\n"
                 numargs
                 (Sexp.to_string (Sexp.List program))
     end

  | Sexp.List (pf :: args :: program) ->
     fprintf stderr "Not a valid postfix program.\n\n";
     print_usage Sys.argv.(0)




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
