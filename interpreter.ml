open Core.Std
open Ast


let do_int_op stack op =
  let v1 = Stack.pop stack in
  let v2 = Stack.pop stack in
  match v1, v2 with
  | None,   Some _
  | Some _, None
  | None,   None ->
     eprintf "Not enough values on the stack\n%!";
     exit 1
  | Some v1, Some v2 ->
     match v1, v2 with
     | IntVal v1, IntVal v2 ->
        Stack.push stack (IntVal (op v1 v2));
        []

     | IntVal _, badval
     | badval,   IntVal _ ->
        eprintf "Got '%s' when expecting integer\n%!"
                (Sexp.to_string (sexp_of_t badval));
        exit 1

     | _, _ ->
        eprintf "Got '%s' and '%s' when expecting integers\n%!"
                (Sexp.to_string (sexp_of_t v1))
                (Sexp.to_string (sexp_of_t v2));
        exit 1

let swap stack =
  let v1 = Stack.pop stack in
  let v2 = Stack.pop stack in
  match v1, v2 with
  | None,   Some _
  | Some _, None
  | None,   None ->
     eprintf "Not enough values on the stack\n%!";
     exit 1
  | Some v1, Some v2 ->
     Stack.push stack v1;
     Stack.push stack v2;
     []

let pop stack =
  match Stack.pop stack with
  | None ->
     eprintf "Not enough values on the stack\n%!";
     exit 1
  | Some _ -> []

let sel stack =
  let v1 = Stack.pop stack in
  let v2 = Stack.pop stack in
  let v3 = Stack.pop stack in
  match v1, v2, v3 with
  | None, None, None
  | Some _, None, None
  | None, Some _, None
  | None, None, Some _
  | Some _, Some _, None
  | Some _, None, Some _
  | None, Some _, Some _ ->
     eprintf "Not enough values on the stack\n%!";
     exit 1

  | Some v1, Some v2, Some v3 ->
     match v3 with
     | IntVal v3 ->
        if v3 = 0 then Stack.push stack v1
        else Stack.push stack v2;
        []
     | _ ->
        eprintf"Got '%s' when expecting an integer\n%!"
               (Sexp.to_string (sexp_of_t v3));
        exit 1

let nget stack =
  match Stack.pop stack with
  | None ->
     eprintf "Not enough values on the stack\n%!";
     exit 1
  | Some ((Command _) as v)
  | Some ((ExecSeq _) as v) ->
     eprintf "Got '%s' when expecting an integer\n%!"
             (Sexp.to_string (sexp_of_t v));
     exit 1

  | Some (IntVal vi) ->
     let slist = Stack.to_list stack in
     match List.nth slist (vi - 1) with
     | None ->
        eprintf "Not enough values on the stack\n%!";
        exit 1
     | Some vnth ->
        Stack.push stack vnth; []

let exec stack =
  match Stack.pop stack with
  | None ->
     eprintf "Not enough values on the stack\n%!";
     exit 1

  | Some ((IntVal _)  as v)
  | Some ((Command _) as v) ->
     eprintf "Got '%s' when expecting an integer\n%!"
             (Sexp.to_string (sexp_of_t v));
     exit 1

  | Some (ExecSeq seq) ->
     seq

let do_command stack = function
  | Add ->  do_int_op stack (fun v1 v2 -> v2 + v1)
  | Sub ->  do_int_op stack (fun v1 v2 -> v2 - v1)
  | Mul ->  do_int_op stack (fun v1 v2 -> v2 * v1)
  | Div ->  do_int_op stack (fun v1 v2 -> v2 / v1)
  | Rem ->  do_int_op stack (fun v1 v2 -> v2 % v1)
  | Eq ->   do_int_op stack (fun v1 v2 -> if v2 = v1 then 1 else 0)
  | Gt ->   do_int_op stack (fun v1 v2 -> if v2 > v1 then 1 else 0)
  | Lt ->   do_int_op stack (fun v1 v2 -> if v2 < v1 then 1 else 0)
  | Swap -> swap stack
  | Pop ->  pop stack
  | Sel ->  sel stack
  | Nget -> nget stack
  | Exec -> exec stack


let rec process stack cmds =
  match cmds with
  | [] -> Stack.pop stack
  | hd :: tl ->
     begin
       match hd with
       | IntVal _
       | ExecSeq _ ->
          Stack.push stack hd;
          process stack tl
       | Command cmd ->
          let newcmds = do_command stack cmd in
          let cmds = List.append newcmds tl in
          process stack cmds
     end


let run numargs args cmds =
  let args, _  = List.split_n args numargs in
  let args     = List.map ~f:(fun e -> IntVal e) args in
  let stack    = Stack.of_list args in
  match process stack cmds with
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
