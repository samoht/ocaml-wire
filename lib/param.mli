type input
type output
type ('a, 'k) t
type env

val input : string -> 'a Types.typ -> ('a, input) t
val output : string -> 'a Types.typ -> ('a, output) t
val decl : ('a, 'k) t -> Types.param
val empty : env
val is_empty : env -> bool
val bind : env -> ('a, input) t -> 'a -> env
val init : env -> ('a, output) t -> 'a -> env
val get : env -> ('a, 'k) t -> 'a
val to_ctx : env -> (string * int) list
val store_name : env -> string -> int -> unit
