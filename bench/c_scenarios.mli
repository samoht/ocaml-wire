(** Pure C scenario benchmark loops. *)

val routing : bytes -> int -> int
(** [routing buf n] runs APID demux on [n] packets in [buf]. Returns ns. *)

val routing_counts : int -> int
(** [routing_counts i] returns the dispatch count for handler [i]. *)

val gateway : bytes -> int -> int -> int
(** [gateway buf n cadu_size] runs TM frame reassembly. Returns ns. *)

val gateway_pkts : unit -> int
(** [gateway_pkts ()] returns the packet count from the last [gateway] run. *)

val clcw_contiguous : bytes -> int -> int
(** [clcw_contiguous buf n] runs CLCW polling on [n] packed 4-byte words in
    contiguous [buf]. Returns ns. *)

val clcw_anomalies : unit -> int
(** [clcw_anomalies ()] returns the anomaly count from the last [clcw] run. *)
