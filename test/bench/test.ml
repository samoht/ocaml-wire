let () =
  Alcotest.run "bench"
    (Test_bench_lib.suites @ Test_application_benches.suites
   @ Test_demo_bench_cases.suites)
