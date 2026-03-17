(** OCaml externals for pure C scenario benchmark loops. *)

external routing : bytes -> int -> int = "ep_c_routing"
external routing_counts : int -> int = "ep_c_routing_counts" [@@noalloc]
external gateway : bytes -> int -> int -> int = "ep_c_gateway_with_pkts"
external gateway_pkts : unit -> int = "ep_c_gateway_pkts" [@@noalloc]
external clcw_contiguous : bytes -> int -> int = "ep_c_clcw_contiguous"
external clcw_anomalies : unit -> int = "ep_c_clcw_anomalies" [@@noalloc]
