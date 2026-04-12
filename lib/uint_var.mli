(** Variable-width unsigned integer read/write. *)

val read : Types.endian -> bytes -> int -> int -> int
(** [read endian buf off size] reads [size] bytes as an unsigned int. *)

val write : Types.endian -> bytes -> int -> int -> int -> unit
(** [write endian buf off size v] writes [v] as [size] bytes. *)
