(** Tests for Wire_stubs — OCaml FFI stub generation.

    Every test that generates ML stubs compiles them with ocamlfind to catch
    arity mismatches, syntax errors, and type errors in the generated code. *)

open Wire
open Wire.C.Raw

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(** Compile a generated ML stub to verify it is syntactically and type-valid. *)
let compile_ml_stub ml =
  let dir = Filename.temp_dir "wire_stub_test" "" in
  let ml_path = Filename.concat dir "stub.ml" in
  let cmi_path = Filename.concat dir "stub.cmi" in
  let oc = open_out ml_path in
  output_string oc ml;
  close_out oc;
  let cmd =
    Fmt.str "ocamlfind ocamlc -package wire -c %s -o %s 2>&1" ml_path cmi_path
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  (try Sys.remove ml_path with _ -> ());
  (try Sys.remove cmi_path with _ -> ());
  (try
     Sys.remove
       (Filename.concat dir
          (Filename.chop_extension (Filename.basename ml_path) ^ ".cmo"))
   with _ -> ());
  (try Sys.rmdir dir with _ -> ());
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.failf "stub compilation failed:\n%s\nGenerated:\n%s" output ml

(** Generate both to_ml_stub and to_ml_stubs, compile both, check agreement. *)
let check_struct s =
  let single = Wire_stubs.to_ml_stub s in
  let multi = Wire_stubs.to_ml_stubs [ s ] in
  compile_ml_stub single;
  compile_ml_stub multi;
  (* Both must have the same type signature (modulo function name) *)
  let params = struct_params s in
  let input_params =
    List.filter (fun p -> not (Wire.Private.param_is_mutable p)) params
  in
  let output_params =
    List.filter (fun p -> Wire.Private.param_is_mutable p) params
  in
  let n_inputs = List.length input_params in
  let n_outputs = List.length output_params in
  (* Count arrows in signatures *)
  let count_arrows s =
    let r = Re.compile (Re.str "->") in
    List.length (Re.all r s)
  in
  let single_arrows = count_arrows single in
  let multi_arrows = count_arrows multi in
  let expected = 1 + n_inputs + n_outputs in
  Alcotest.(check int) "single arity" expected single_arrows;
  Alcotest.(check int) "multi arity" expected multi_arrows;
  if n_inputs > 0 then
    Alcotest.(check bool)
      "single has int ->" true
      (contains ~sub:"int ->" single);
  if n_outputs > 0 then
    Alcotest.(check bool)
      "single has int array" true
      (contains ~sub:"int array" single);
  let n_args = 1 + n_inputs + n_outputs in
  if n_args > 5 then begin
    Alcotest.(check bool)
      "single has bytecode" true
      (contains ~sub:"_bytecode" single);
    Alcotest.(check bool)
      "multi has bytecode" true
      (contains ~sub:"_bytecode" multi)
  end

(* ── No params ── *)

let test_no_params () =
  check_struct
    (struct_ "SimpleHeader"
       [ field "version" uint8; field "length" uint16; field "flags" uint8 ])

let test_single_field () = check_struct (struct_ "Tiny" [ field "x" uint8 ])

let test_bitfield_struct () =
  check_struct
    (struct_ "BitFields"
       [
         field "a" (bits ~width:3 U8);
         field "b" (bits ~width:5 U8);
         field "c" (bits ~width:16 U32be);
       ])

let test_many_fields () =
  check_struct
    (struct_ "Wide"
       [
         field "a" uint8;
         field "b" uint16be;
         field "c" uint32be;
         field "d" uint8;
         field "e" uint16be;
         field "f" uint32be;
         field "g" uint8;
         field "h" uint16be;
       ])

(* ── Input params only ── *)

let test_one_input () =
  check_struct
    (param_struct "OneInput" [ param "limit" uint8 ] [ field "x" uint8 ])

let test_two_inputs () =
  check_struct
    (param_struct "TwoInputs"
       [ param "lo" uint8; param "hi" uint16be ]
       [ field "x" uint8 ])

(* ── Output params only ── *)

let test_one_output () =
  check_struct
    (param_struct "OneOutput"
       [ mutable_param "out" uint16be ]
       [ field "x" uint8 ])

let test_two_outputs () =
  check_struct
    (param_struct "TwoOutputs"
       [ mutable_param "out_a" uint8; mutable_param "out_b" uint16be ]
       [ field "x" uint8 ])

(* ── Mixed input + output ── *)

let test_mixed () =
  check_struct
    (param_struct "Mixed"
       [ param "max_len" uint16be; mutable_param "out_len" uint16be ]
       [ field "Length" uint16be ])

let test_mixed_many () =
  check_struct
    (param_struct "MixedMany"
       [
         param "a" uint8;
         param "b" uint8;
         mutable_param "out_a" uint8;
         mutable_param "out_b" uint16be;
       ]
       [ field "x" uint8 ])

(* ── >5 args (bytecode path) ── *)

let test_many_params_bytecode () =
  check_struct
    (param_struct "ManyParams"
       [
         param "a" uint8;
         param "b" uint8;
         param "c" uint8;
         param "d" uint8;
         param "e" uint8;
         mutable_param "out" uint8;
       ]
       [ field "x" uint8 ])

let test_exactly_five_args () =
  (* bytes + 4 inputs = 5 args, should NOT trigger bytecode *)
  let s =
    param_struct "FiveArgs"
      [ param "a" uint8; param "b" uint8; param "c" uint8; param "d" uint8 ]
      [ field "x" uint8 ]
  in
  check_struct s;
  let ml = Wire_stubs.to_ml_stub s in
  Alcotest.(check bool)
    "no bytecode at exactly 5" false
    (contains ~sub:"_bytecode" ml)

let test_six_args_bytecode () =
  (* bytes + 5 inputs = 6 args, SHOULD trigger bytecode *)
  let s =
    param_struct "SixArgs"
      [
        param "a" uint8;
        param "b" uint8;
        param "c" uint8;
        param "d" uint8;
        param "e" uint8;
      ]
      [ field "x" uint8 ]
  in
  check_struct s;
  let ml = Wire_stubs.to_ml_stub s in
  Alcotest.(check bool) "bytecode at 6" true (contains ~sub:"_bytecode" ml)

(* ── Bitfield params ── *)

let test_bitfield_param () =
  check_struct
    (param_struct "BfParam"
       [ param "mask" (bits ~width:4 U8) ]
       [ field "v" uint8 ])

(* ── Multiple structs in one to_ml_stubs call ── *)

let test_multiple_structs () =
  let s1 = struct_ "Foo" [ field "x" uint8 ] in
  let s2 = struct_ "Bar" [ field "y" uint16be ] in
  let s3 = param_struct "Baz" [ param "limit" uint8 ] [ field "z" uint32be ] in
  let ml = Wire_stubs.to_ml_stubs [ s1; s2; s3 ] in
  compile_ml_stub ml;
  Alcotest.(check bool) "has foo" true (contains ~sub:"foo_check" ml);
  Alcotest.(check bool) "has bar" true (contains ~sub:"bar_check" ml);
  Alcotest.(check bool) "has baz" true (contains ~sub:"baz_check" ml);
  (* baz has params *)
  Alcotest.(check bool) "baz has int" true (contains ~sub:"int ->" ml)

(* ── C stubs (string checks, cannot compile without EverParse headers) ── *)

let test_c_stubs_no_params () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "has check" true
    (contains ~sub:"caml_wire_simpleheader_check" c);
  Alcotest.(check bool)
    "has error handler" true
    (contains ~sub:"simpleheader_err" c);
  Alcotest.(check bool) "has EverParse.h" true (contains ~sub:"EverParse.h" c)

let test_c_stubs_with_params () =
  let s =
    param_struct "Bounded"
      [ param "max_len" uint16be; mutable_param "out_len" uint16be ]
      [ field "Length" uint16be ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "has check" true
    (contains ~sub:"caml_wire_bounded_check" c);
  Alcotest.(check bool) "has max_len local" true (contains ~sub:"max_len_val" c);
  Alcotest.(check bool) "has out_len local" true (contains ~sub:"out_len_val" c);
  Alcotest.(check bool) "has Store_field" true (contains ~sub:"Store_field" c)

let test_c_stubs_bytecode () =
  let s =
    param_struct "ManyParams"
      [
        param "a" uint8;
        param "b" uint8;
        param "c" uint8;
        param "d" uint8;
        param "e" uint8;
        mutable_param "out" uint8;
      ]
      [ field "x" uint8 ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool) "has bytecode" true (contains ~sub:"_bytecode" c);
  Alcotest.(check bool) "has argv" true (contains ~sub:"argv" c)

(* ── to_ml_stub_name ── *)

let test_name_camel () =
  Alcotest.(check string)
    "camel" "simple_header"
    (Wire_stubs.to_ml_stub_name (struct_ "SimpleHeader" [ field "x" uint8 ]))

let test_name_allcaps () =
  Alcotest.(check string)
    "allcaps" "c_l_c_w"
    (Wire_stubs.to_ml_stub_name (struct_ "CLCW" [ field "x" uint8 ]))

let test_name_single () =
  Alcotest.(check string)
    "single" "foo"
    (Wire_stubs.to_ml_stub_name (struct_ "Foo" [ field "x" uint8 ]))

(* ── End-to-end: generate → EverParse → compile C+ML → call ── *)

let has_3d = Wire_3d.has_3d_exe ()

let run cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.failf "command failed: %s\n%s" cmd output

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

(** End-to-end: .3d → EverParse → C stubs → ML stubs → compile → call. [test_ml]
    is the OCaml source that calls the generated stubs and exits 0 on success,
    non-zero on failure. *)
let e2e ~name ~structs ~module_ ~test_ml =
  if not has_3d then ()
  else begin
    let dir = Filename.temp_dir ("wire_e2e_" ^ name) "" in
    (* 1. Generate .3d and run EverParse *)
    let schema =
      Wire_3d.schema ~name ~module_ ~wire_size:0
      (* wire_size unused for generation *)
    in
    Wire_3d.generate_3d ~outdir:dir [ schema ];
    Wire_3d.run_everparse ~outdir:dir [ schema ];
    (* 2. Generate C and ML stubs *)
    let c_stubs = Wire_stubs.to_c_stubs structs in
    let ml_stubs = Wire_stubs.to_ml_stubs structs in
    write_file (Filename.concat dir "wire_ffi.c") c_stubs;
    write_file (Filename.concat dir "stubs.ml") ml_stubs;
    write_file (Filename.concat dir "main.ml") test_ml;
    (* 3. Compile *)
    let cmd =
      Fmt.str
        "cd %s && ocamlfind ocamlopt -package wire -linkpkg -ccopt '-I %s' \
         wire_ffi.c stubs.ml main.ml -o test_e2e 2>&1"
        dir dir
    in
    run cmd;
    (* 4. Run *)
    run (Filename.concat dir "test_e2e")
  end

let test_e2e_no_params () =
  let s =
    struct_ "TestHeader" [ field "version" uint8; field "length" uint16be ]
  in
  let m = module_ [ typedef ~entrypoint:true s ] in
  e2e ~name:"TestHeader" ~structs:[ s ] ~module_:m
    ~test_ml:
      {|let () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 42;
  assert (Stubs.testheader_check buf);
  (* Truncated buffer should fail *)
  assert (not (Stubs.testheader_check (Bytes.create 1)))
|}

let test_e2e_with_constraint () =
  let f_x = field "x" uint8 in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(field_ref f_x <= int 100) uint8 ]
  in
  let m = module_ [ typedef ~entrypoint:true s ] in
  e2e ~name:"Constrained" ~structs:[ s ] ~module_:m
    ~test_ml:
      {|let () =
  let buf = Bytes.create 1 in
  (* x=50 <= 100: passes *)
  Bytes.set_uint8 buf 0 50;
  assert (Stubs.constrained_check buf);
  (* x=200 > 100: fails *)
  Bytes.set_uint8 buf 0 200;
  assert (not (Stubs.constrained_check buf))
|}

let test_e2e_bitfields () =
  let s =
    struct_ "BfHeader"
      [
        field "version" (bits ~width:4 U8);
        field "flags" (bits ~width:4 U8);
        field "length" uint16be;
      ]
  in
  let m = module_ [ typedef ~entrypoint:true s ] in
  e2e ~name:"BfHeader" ~structs:[ s ] ~module_:m
    ~test_ml:
      {|let () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 0x45;
  Bytes.set_uint16_be buf 1 100;
  assert (Stubs.bfheader_check buf);
  assert (not (Stubs.bfheader_check (Bytes.create 2)))
|}

(* ── Suite ── *)

let suite =
  ( "wire_stubs",
    [
      (* ML stubs — no params *)
      Alcotest.test_case "no params" `Quick test_no_params;
      Alcotest.test_case "single field" `Quick test_single_field;
      Alcotest.test_case "bitfield struct" `Quick test_bitfield_struct;
      Alcotest.test_case "many fields" `Quick test_many_fields;
      (* ML stubs — input params *)
      Alcotest.test_case "one input" `Quick test_one_input;
      Alcotest.test_case "two inputs" `Quick test_two_inputs;
      (* ML stubs — output params *)
      Alcotest.test_case "one output" `Quick test_one_output;
      Alcotest.test_case "two outputs" `Quick test_two_outputs;
      (* ML stubs — mixed *)
      Alcotest.test_case "mixed input+output" `Quick test_mixed;
      Alcotest.test_case "mixed many" `Quick test_mixed_many;
      (* ML stubs — bytecode threshold *)
      Alcotest.test_case "many params (bytecode)" `Quick
        test_many_params_bytecode;
      Alcotest.test_case "exactly 5 args (no bytecode)" `Quick
        test_exactly_five_args;
      Alcotest.test_case "6 args (bytecode)" `Quick test_six_args_bytecode;
      (* ML stubs — other *)
      Alcotest.test_case "bitfield param" `Quick test_bitfield_param;
      Alcotest.test_case "multiple structs" `Quick test_multiple_structs;
      (* C stubs *)
      Alcotest.test_case "c stubs: no params" `Quick test_c_stubs_no_params;
      Alcotest.test_case "c stubs: with params" `Quick test_c_stubs_with_params;
      Alcotest.test_case "c stubs: bytecode" `Quick test_c_stubs_bytecode;
      (* stub name *)
      Alcotest.test_case "name: camelCase" `Quick test_name_camel;
      Alcotest.test_case "name: ALLCAPS" `Quick test_name_allcaps;
      Alcotest.test_case "name: single word" `Quick test_name_single;
      (* end-to-end: generate → EverParse → compile → call *)
      Alcotest.test_case "e2e: no params" `Slow test_e2e_no_params;
      Alcotest.test_case "e2e: constraint" `Slow test_e2e_with_constraint;
      Alcotest.test_case "e2e: bitfields" `Slow test_e2e_bitfields;
    ] )
