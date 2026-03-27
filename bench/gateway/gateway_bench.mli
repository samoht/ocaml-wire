(** TM frame reassembly benchmark harness. *)

val check : n_frames:int -> unit
(** [check ~n_frames] verifies that the OCaml and C TM frame reassembly produce
    identical results. *)

val main : unit -> unit
(** [main ()] runs the TM frame reassembly benchmark and prints a comparison
    table. *)
