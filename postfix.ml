open Core.Std

let print_usage pname =
  printf "usage: %s <program>" pname

let () =
  match (Array.to_list Sys.argv) with
  (* If we don't get the right number of arguments, print out the usage
  message. *)
  | [] ->
     print_usage ""
  | pname :: [] ->
     print_usage pname
  | _ :: program :: [] ->
     ()
  | pname :: _ :: _ ->
     print_usage pname
