(** Shared bitfield utilities. *)

val byte_size : Types.bitfield_base -> int

val total_bits : Types.bitfield_base -> int
(** Total bits in a bitfield base type (8, 16, or 32). *)

val equal : Types.bitfield_base -> Types.bitfield_base -> bool
(** Check if two bitfield bases are the same type and endianness. *)

val read_word : Types.bitfield_base -> bytes -> int -> int
(** Read the base word from bytes at offset. *)

val write_word : Types.bitfield_base -> bytes -> int -> int -> unit
(** Write the base word to bytes at offset. *)

val extract : total:int -> bits_used:int -> width:int -> int -> int
(** Extract bits from a word given total, bits_used, and width. *)
