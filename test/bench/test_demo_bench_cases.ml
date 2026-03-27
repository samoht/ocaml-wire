open Demo_bench_cases

let test_write_cases () =
  List.iter (fun case -> case.verify ()) write_benchmark_cases

let test_projection_case_ids_unique () =
  let seen = Hashtbl.create 17 in
  List.iter
    (fun (C (Read_case case)) ->
      if Hashtbl.mem seen case.id then
        Alcotest.failf "duplicate projection case id";
      Hashtbl.add seen case.id ())
    projection_cases

let test_read_cases_derived () =
  let read_ids =
    List.map (fun (C (Read_case c)) -> c.id) read_benchmark_cases
  in
  let expected =
    List.filter_map
      (fun (C (Read_case c)) -> if c.bench_read then Some c.id else None)
      projection_cases
  in
  Alcotest.(check (list int))
    "read_benchmark_cases = filter bench_read projection_cases"
    (List.map Hashtbl.hash expected)
    (List.map Hashtbl.hash read_ids)

let suite =
  ( "demo_bench_cases",
    [
      Alcotest.test_case "write verifiers" `Quick test_write_cases;
      Alcotest.test_case "projection ids unique" `Quick
        test_projection_case_ids_unique;
      Alcotest.test_case "read cases derived" `Quick test_read_cases_derived;
    ] )
