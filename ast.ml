open Core.Std

type command =
  | Add | Sub | Mul | Div | Rem
  | Eq  | Gt  | Lt
  | Pop | Sel | Swap | Nget | Exec with sexp

type t =
  | Command of command
  | IntVal of int
  | ExecSeq of t list with sexp


let rec parse_from_sexp sexp =
  match sexp with
  | Sexp.Atom e ->
     begin
       match Option.try_with (fun () -> Int.of_string e) with
       | None ->
          begin
            match Option.try_with (fun () -> command_of_sexp sexp) with
            | None -> failwith (sprintf "Encountered unexpected value %s" e)
            | Some cmd ->
               Command cmd
          end

       | Some num ->
          IntVal num
     end

  | Sexp.List _ ->
     ExecSeq (List.t_of_sexp parse_from_sexp sexp)
