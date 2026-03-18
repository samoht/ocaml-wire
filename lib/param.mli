type 'a t
type binding

val input : string -> 'a Types.typ -> 'a t
val output : string -> 'a Types.typ -> 'a t
val spec : 'a t -> Types.param
val value : 'a t -> 'a -> binding
val slot : 'a t -> 'a ref -> binding
val name : binding -> string
val load : binding -> int
val store : binding -> int -> unit
