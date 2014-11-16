open Core.Std
open Ast

let stack = Stack.create ()


let do_command = function
  | Add -> ()
  | Sub -> ()
  | Mul -> ()
  | Div -> ()
  | Rem -> ()
  | Eq ->  ()
  | Gt ->  ()
  | Lt ->  ()
  | Pop -> let _ = Stack.pop stack in ()
  | Sel ->  ()
  | Swap -> ()
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
