(** Benchmark schemas covering all Wire API constructs. *)

(** Type-erased schema for benchmark iteration. *)
type 'a schema = private {
  name : string;
  codec : 'a Wire.Codec.t;
  struct_ : Wire.struct_;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}

type any_schema = Any : 'a schema -> any_schema

val all_schemas : any_schema list
val all_structs : Wire.struct_ list
