(** Tests for Diff_gen — EverParse differential test generation. *)

open Wire

let simple_struct =
  struct_ "TestDiffGen" [ field "A" uint8; field "B" uint16be ]

let simple_module =
  module_ "TestDiffGen" [ typedef ~entrypoint:true simple_struct ]

let test_schema_creation () =
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
  in
  Alcotest.(check bool) "some" true (Option.is_some s)

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "diff_gen_test" "" in
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
    |> Option.get
  in
  Wire_diff_gen.generate_3d_files ~schema_dir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestDiffGen.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let suite =
  ( "wire_diff_gen",
    [
      Alcotest.test_case "schema creation" `Quick test_schema_creation;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
    ] )
