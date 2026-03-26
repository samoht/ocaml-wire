type case = {
  id : Demo_bench_cases.id;
  label : string;
  packed : Wire_diff.packed_test;
}

val cases : case list
val verify_of_id : Demo_bench_cases.id -> unit -> unit
