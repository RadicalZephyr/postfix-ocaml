open Core.Std

let print_usage pname =
  printf "usage: %s <program>" pname


let compile program =
  match Sexp.of_string program with
  | Sexp.Atom _
  | Sexp.List ([])
  | Sexp.List (_ :: [])
  | Sexp.List (_ :: _ :: []) ->
     fprintf stderr "Not a valid postfix program."

  | Sexp.List ((Atom "postfix") :: (Atom args) :: program) ->
     printf "Valid postfix program of %d args: %s"
            (Int.of_string args)
            (Sexp.to_string (Sexp.List program))

  | Sexp.List (pf :: args :: program) ->
     fprintf stderr "Not a valid postfix program."



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
