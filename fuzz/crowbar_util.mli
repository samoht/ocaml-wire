(** Alcotest-style wrapper around Crowbar for fuzz testing. *)

type test_case = unit -> unit
(** A single fuzz test case. *)

val test_case : string -> ('f, unit) Crowbar.gens -> 'f -> test_case
(** [test_case name gens f] creates a fuzz test using generators [gens]. *)

val run : string -> (string * test_case list) list -> unit
(** [run name suites] runs all fuzz test suites under [name]. *)
