(** Differential testing: one {!Wire.Codec.t} against external C behaviour.

    A diff test keeps the codec as the single OCaml authority and compares it
    against C read/write functions, typically backed by EverParse-generated
    code. *)

type 'r harness
(** A test harness bundling a codec with C read/write oracles. *)

val harness :
  name:string ->
  codec:'r Wire.Codec.t ->
  c_read:(string -> string option) ->
  c_write:(string -> string option) ->
  equal:('r -> 'r -> bool) ->
  'r harness
(** [harness ~name ~codec ~c_read ~c_write ~equal] creates a test harness. *)

type result =
  | Match
  | Both_failed
  | Value_mismatch of string
  | Only_c_ok of string
  | Only_ocaml_ok of string

val read : 'r harness -> string -> result
(** [read h buf] parses [buf] with both OCaml and C, compares results. *)

val write : 'r harness -> 'r -> result
(** [write h v] encodes [v] with OCaml, roundtrips through C, compares. *)

val full_roundtrip : 'r harness -> 'r -> result
(** [full_roundtrip h v] encodes with OCaml, roundtrips through C, decodes with
    OCaml, and compares the final value to the original. *)

(** {1 Type-erased testing} *)

type packed_test = {
  name : string;
  wire_size : int;
  test_read : string -> result;
  test_write : string -> result;
  test_roundtrip : string -> result;
}
(** A type-erased test for generic test loops. *)

val pack : 'r harness -> packed_test
(** [pack h] creates a type-erased test from a harness. *)
