open Core.Std
open Ast

let stack = Stack.create ()


let do_int_op op =
  let v1 = Stack.pop stack in
  let v2 = Stack.pop stack in
  match (v1, v2) with
  | ((Some (IntVal v1)), (Some (IntVal v2))) ->
     Stack.push stack (IntVal (op v1 v2))

  | ((Some (IntVal _)),  (Some  badval))
  | ((Some  badval),     (Some (IntVal _))) ->
     eprintf "Got '%s' when expecting integer\n%!"
             (Sexp.to_string (sexp_of_t badval));
     exit 1

  | (Some v1, Some v2) ->
     eprintf "Got '%s' and '%s' when expecting integers\n%!"
             (Sexp.to_string (sexp_of_t v1))
             (Sexp.to_string (sexp_of_t v2));
     exit 1
  | (None,   Some _)
  | (Some _, None)
  | (None, None) ->
     eprintf "Not enough values on the stack\n%!";
     exit 1

let do_command = function
  | Add ->  do_int_op (fun v1 v2 -> v2 + v1)
  | Sub ->  do_int_op (fun v1 v2 -> v2 - v1)
  | Mul ->  do_int_op (fun v1 v2 -> v2 * v1)
  | Div ->  do_int_op (fun v1 v2 -> v2 / v1)
  | Rem ->  do_int_op (fun v1 v2 -> v2 % v1)
  | Eq ->   do_int_op (fun v1 v2 -> if v2 = v1 then 1 else 0)
  | Gt ->   do_int_op (fun v1 v2 -> if v2 < v1 then 1 else 0)
  | Lt ->   do_int_op (fun v1 v2 -> if v2 > v1 then 1 else 0)
  | Pop -> let _ = Stack.pop stack in ()
  | Swap -> do_int_op (fun v1 v2 -> Stack.push stack (IntVal v1);
                                    v2)
  | Sel ->  ()
  | Nget -> ()
  | Exec -> ()


let rec process cmds =
  match cmds with
  | [] -> Stack.pop stack
  | hd :: tl ->
     begin
       match hd with
       | IntVal _
       | ExecSeq _ ->
          Stack.push stack hd;
       | Command cmd ->
          do_command cmd
     end;
     process tl

let run numargs args cmds =
  let rec push_args count  = function
    | [] -> ()
    | hd :: tl ->
       push_args (count - 1) tl;
       Stack.push stack (IntVal hd)
  in

  push_args numargs args;
  match process cmds with
  | None ->
     printf "Error: Final stack was empty\n%!";
     ()
  | Some (ExecSeq _)
  | Some (Command _) ->
     printf "Error: Final stack value was not an integer\n%!";
     ()
  | Some (IntVal result) ->
     printf "Result: '%d'\n%!" result;
     ()
