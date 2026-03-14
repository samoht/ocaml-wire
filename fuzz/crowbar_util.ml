(** Thin Alcotest-style wrapper over upstream Crowbar.

    Provides [test_case] and [run] for grouping fuzz tests into named suites,
    built on top of [Crowbar.add_test]. *)

type test_case = unit -> unit

let test_case (type f) name (gens : (f, unit) Crowbar.gens) (f : f) : test_case =
  fun () -> Crowbar.add_test ~name gens f

let run _suite_name (groups : (string * test_case list) list) =
  List.iter
    (fun (_group_name, cases) -> List.iter (fun tc -> tc ()) cases)
    groups
