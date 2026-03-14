type test_case = unit -> unit

val test_case : string -> ('f, unit) Crowbar.gens -> 'f -> test_case
val run : string -> (string * test_case list) list -> unit
