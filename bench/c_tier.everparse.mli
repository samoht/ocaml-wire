(** EverParse C-tier benchmark loops. *)

val spacepacket_loop : (bytes -> int -> int -> int) option
(** SpacePacket EverParse validator loop. *)

val tmframe_loop : (bytes -> int -> int -> int) option
(** TM frame EverParse validator loop. *)

val clcw_loop : (bytes -> int -> int -> int) option
(** CLCW EverParse validator loop. *)
