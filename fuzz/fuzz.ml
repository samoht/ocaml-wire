let () = Alcobar.run "wire" [ Fuzz_wire.suite; Fuzz_c.suite; Fuzz_param.suite ]
