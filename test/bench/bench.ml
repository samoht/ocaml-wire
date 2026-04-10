let bench_routing () = Routing_bench.check ~n_pkts:10_000
let bench_clcw () = Clcw_bench.check ~n_words:10_000
let bench_gateway () = Gateway_bench.check ~n_frames:1_000

let () =
  Alcotest.run "bench"
    [
      ( "bench",
        [
          Alcotest.test_case "verify: routing counts" `Quick bench_routing;
          Alcotest.test_case "verify: clcw anomalies" `Quick bench_clcw;
          Alcotest.test_case "verify: gateway checksums" `Quick bench_gateway;
        ] );
    ]
