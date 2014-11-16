open Core.Std


let stack = Stack.create ()

let run numargs args ast =
  let rec push_args count  = function
    | [] -> ()
    | hd :: tl ->
       push_args (count - 1) tl;
       Stack.push stack (Ast.IntVal hd)
  in

  push_args numargs args
