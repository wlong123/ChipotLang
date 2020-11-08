open Ast

exception Illegal

let rec eval = function
| Var x -> Var x
| Int i -> Int i
| Float f -> Float f
| Binop (op, e1, e2) -> begin
  match eval e1, eval e2 with
  | Int i1, Int i2 -> Int (i1 + i2)
  | _ -> raise Illegal
end
| _ -> failwith "UNIMPLEMENTED"
