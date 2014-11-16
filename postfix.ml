open Core.Std

let print_usage pname =
  printf "usage: %s <program>\n" pname


let compile program =
  match Sexp.of_string program with
  | Sexp.Atom _
  | Sexp.List ([])
  | Sexp.List (_ :: [])
  | Sexp.List (_ :: _ :: []) ->
     fprintf stderr "Not a valid postfix program.\n"

  | Sexp.List ((Atom "postfix") :: (Atom args) :: program) ->
     begin
       match Or_error.try_with (fun () -> Int.of_string args) with

       | Error _ -> fprintf stderr "%s is not an integer.\n" args

       | Ok numargs ->
          printf "Valid postfix program of %d args: '%s'\n"
                 numargs
                 (Sexp.to_string (Sexp.List program))
     end

  | Sexp.List (pf :: args :: program) ->
     fprintf stderr "Not a valid postfix program.\n"



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
