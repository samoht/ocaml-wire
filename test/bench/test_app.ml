let () =
  Alcotest.run "app_bench"
    [
      ( "app_bench",
        [
          Alcotest.test_case "verify: routing counts" `Quick (fun () ->
              Routing_bench.check ~n_pkts:10_000);
          Alcotest.test_case "verify: clcw anomalies" `Quick (fun () ->
              Clcw_bench.check ~n_words:10_000);
          Alcotest.test_case "verify: gateway checksums" `Quick (fun () ->
              Gateway_bench.check ~n_frames:1_000);
        ] );
    ]
