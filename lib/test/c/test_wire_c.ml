(** Tests for Wire_c — C library generation from Wire codecs. *)

open Wire

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module = module_ "TestSimple" [ typedef ~entrypoint:true simple_struct ]

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "wire_c_test" "" in
  let s = Wire_c.schema ~name:"TestSimple" ~module_:simple_module ~wire_size:3 in
  Wire_c.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let suite =
  ( "wire_c",
    [
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
    ] )
