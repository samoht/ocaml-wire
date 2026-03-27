(** APID demux routing benchmark harness. *)

val check : n_pkts:int -> unit
(** [check ~n_pkts] verifies that the OCaml and C APID routing produce identical
    results. *)

val main : unit -> unit
(** [main ()] runs the APID demux throughput benchmark and prints a comparison
    table. *)
