open Core.Std

type command =
  | Add | Sub | Mul | Div | Rem
  | Eq  | Gt  | Lt
  | Pop | Sel | Swap | Nget | Exec with sexp

type t =
  | Command of command
  | Int of int
  | ExecSeq of t list with sexp
