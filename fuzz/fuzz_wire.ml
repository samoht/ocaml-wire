(** Fuzz tests for wire library.

    Tests cover: pretty-printer crash safety, parse crash safety on arbitrary
    input, encode-then-parse roundtrip correctness, record codec roundtrip, and
    3D code generation for all DSL combinators. *)

module Cr = Crowbar
open Crowbar_util
open Wire

(* Silence unused variable warnings for parse error handling *)
let _ = pp_parse_error

(* Helper: encode record to string using Codec API *)
let encode_record_to_string codec v =
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record_from_string codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error (Unexpected_eof { expected = ws; got = String.length s })
  else Ok (Codec.decode codec (Bytes.of_string s) 0)

(** Convert crowbar bytes to OCaml bytes. *)
let to_bytes buf =
  let len = String.length buf in
  let b = Bytes.create len in
  Bytes.blit_string buf 0 b 0 len;
  b

(** Truncate input to reasonable size for protocol messages. *)
let truncate buf =
  let max_len = 1024 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

(** {1 Pretty-printing Tests} *)

let test_pp_uint8 () =
  let _ = Fmt.str "%a" pp_typ uint8 in
  ()

let test_pp_uint16 () =
  let _ = Fmt.str "%a" pp_typ uint16 in
  ()

let test_pp_uint16be () =
  let _ = Fmt.str "%a" pp_typ uint16be in
  ()

let test_pp_uint32 () =
  let _ = Fmt.str "%a" pp_typ uint32 in
  ()

let test_pp_uint32be () =
  let _ = Fmt.str "%a" pp_typ uint32be in
  ()

let test_pp_uint63 () =
  let _ = Fmt.str "%a" pp_typ uint63 in
  ()

let test_pp_uint63be () =
  let _ = Fmt.str "%a" pp_typ uint63be in
  ()

let test_pp_uint64 () =
  let _ = Fmt.str "%a" pp_typ uint64 in
  ()

let test_pp_uint64be () =
  let _ = Fmt.str "%a" pp_typ uint64be in
  ()

let test_pp_bitfield width =
  if width > 0 && width <= 32 then begin
    let t = bits ~width bf_uint32 in
    let _ = Fmt.str "%a" pp_typ t in
    ()
  end

let test_pp_bf_uint8 width =
  let width = (abs width mod 8) + 1 in
  let t = bits ~width bf_uint8 in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_bf_uint16 width =
  let width = (abs width mod 16) + 1 in
  let t = bits ~width bf_uint16 in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_bf_uint16be width =
  let width = (abs width mod 16) + 1 in
  let t = bits ~width bf_uint16be in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_bf_uint32be width =
  let width = (abs width mod 32) + 1 in
  let t = bits ~width bf_uint32be in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_map () =
  let t = map (fun n -> n * 2) (fun n -> n / 2) uint8 in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_bool () =
  let t = bool (bits ~width:1 bf_uint8) in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_cases () =
  let t = cases [ "A"; "B"; "C" ] uint8 in
  let _ = Fmt.str "%a" pp_typ t in
  ()

let test_pp_unit () =
  let _ = Fmt.str "%a" pp_typ unit in
  ()

(** Test pp_module doesn't crash on valid modules. *)
let test_pp_module_simple () =
  let s = struct_ "Test" [ field "a" uint8; field "b" uint16 ] in
  let m = module_ "Test" [ typedef s ] in
  let _ = to_3d m in
  ()

(** {1 3D Code Generation Tests} *)

(** Test struct with random field count. *)
let test_struct_random_fields n =
  let n = (n mod 10) + 1 in
  let fields = List.init n (fun i -> field (Fmt.str "f%d" i) uint8) in
  let s = struct_ "Random" fields in
  let m = module_ "Random" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test enum with random cases. *)
let test_enum_random_cases n =
  let n = (n mod 10) + 1 in
  let cases = List.init n (fun i -> (Fmt.str "C%d" i, i)) in
  let e = enum_decl "RandEnum" cases uint8 in
  let m = module_ "RandEnum" [ e ] in
  let _ = to_3d m in
  ()

(** Test casetype with random cases. *)
let test_casetype_random n =
  let n = (n mod 5) + 1 in
  let cases = List.init n (fun i -> decl_case i uint8) in
  let ct = casetype_decl "_RandCase" [ param "tag" uint8 ] uint8 cases in
  let m = module_ "RandCase" [ ct ] in
  let _ = to_3d m in
  ()

(** Test inline casetype. *)
let test_casetype_inline () =
  let t =
    casetype "Tag" uint8 [ case 0 uint16; case 1 uint32; default uint8 ]
  in
  let s = struct_ "WithCase" [ field "tag" uint8; field "data" t ] in
  let m = module_ "WithCase" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test constraint expression generation. *)
let test_constraint_expr a =
  let v = abs a mod 1000 in
  let cond = Expr.(ref "x" <= int v) in
  let s = struct_ "Constrained" [ field "x" ~constraint_:cond uint16 ] in
  let m = module_ "Constrained" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test bitfield constraints. *)
let test_bitfield_constraint width =
  let width = (width mod 16) + 1 in
  let t = bits ~width bf_uint16 in
  let cond = Expr.(ref "x" <= int 100) in
  let s = struct_ "BFConstrained" [ field "x" ~constraint_:cond t ] in
  let m = module_ "BFConstrained" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test bitwise expression operators in 3D output. *)
let test_bitwise_expr a =
  let v = abs a mod 256 in
  let open Expr in
  let cond = ref "x" land int 0xFF <= int v in
  let s = struct_ "Bitwise" [ field "x" ~constraint_:cond uint16 ] in
  let m = module_ "Bitwise" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test logical expression operators. *)
let test_logical_expr () =
  let open Expr in
  let cond = ref "x" <= int 100 && ref "x" >= int 0 in
  let s = struct_ "Logical" [ field "x" ~constraint_:cond uint8 ] in
  let m = module_ "Logical" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test all bitwise/shift operators. *)
let test_bitwise_ops () =
  let open Expr in
  let _ = ref "x" lor int 1 in
  let _ = ref "x" lxor int 0xFF in
  let _ = lnot (ref "x") in
  let _ = ref "x" lsl int 2 in
  let _ = ref "x" lsr int 3 in
  ()

(** Test logical operators. *)
let test_logical_ops () =
  let open Expr in
  let _ = true_ || false_ in
  let _ = Expr.not true_ in
  let _ = ref "x" = int 0 || ref "x" <> int 1 in
  ()

(** Test cast operators in 3D output. *)
let test_cast_expr () =
  let open Expr in
  let cond = to_uint8 (ref "x") <= int 100 in
  let s = struct_ "Cast" [ field "x" ~constraint_:cond uint16 ] in
  let m = module_ "Cast" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test all cast variants. *)
let test_cast_variants () =
  let open Expr in
  let _ = to_uint8 (int 42) in
  let _ = to_uint16 (int 42) in
  let _ = to_uint32 (int 42) in
  let _ = to_uint64 (int 42) in
  ()

(** Test sizeof expression. *)
let test_sizeof_expr () =
  let _ = sizeof uint32 in
  let _ = sizeof_this in
  let _ = field_pos in
  ()

(** Test array types. *)
let test_array_type len =
  let len = (abs len mod 100) + 1 in
  let arr = array ~len:(int len) uint8 in
  let s = struct_ "WithArray" [ field "arr" arr ] in
  let m = module_ "WithArray" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test byte_array types. *)
let test_byte_array size =
  let size = (abs size mod 1000) + 1 in
  let ba = byte_array ~size:(int size) in
  let s = struct_ "WithByteArray" [ field "data" ba ] in
  let m = module_ "WithByteArray" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test single_elem_array. *)
let test_single_elem_array () =
  let t = single_elem_array ~size:(int 4) uint32 in
  let s = struct_ "WithSingle" [ field "x" t ] in
  let m = module_ "WithSingle" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test single_elem_array_at_most. *)
let test_single_elem_array_at_most () =
  let t = single_elem_array_at_most ~size:(int 8) uint32 in
  let s = struct_ "WithAtMost" [ field "x" t ] in
  let m = module_ "WithAtMost" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test anon_field (padding). *)
let test_anon_field () =
  let s =
    struct_ "WithPadding"
      [ field "x" uint8; anon_field uint8; field "y" uint16 ]
  in
  let m = module_ "WithPadding" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test parameterized struct. *)
let test_param_struct n =
  let n = (n mod 5) + 1 in
  let params = List.init n (fun i -> param (Fmt.str "p%d" i) uint32) in
  let ps = param_struct "Parametric" params [ field "x" uint8 ] in
  let m = module_ "Parametric" [ typedef ps ] in
  let _ = to_3d m in
  ()

(** Test mutable_param. *)
let test_mutable_param () =
  let ps =
    param_struct "MutParam" [ mutable_param "out" uint32 ] [ field "x" uint8 ]
  in
  let m = module_ "MutParam" [ typedef ps ] in
  let _ = to_3d m in
  ()

(** Test apply (parameterized type application). *)
let test_apply () =
  let t = apply (type_ref "Param") [ int 42 ] in
  let s = struct_ "WithApply" [ field "x" t ] in
  let m = module_ "WithApply" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test type_ref. *)
let test_type_ref () =
  let t : int typ = type_ref "SomeType" in
  let s = struct_ "WithRef" [ field "x" t ] in
  let m = module_ "WithRef" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test qualified_ref. *)
let test_qualified_ref () =
  let t : int typ = qualified_ref "Other" "SomeType" in
  let s = struct_ "WithQRef" [ field "x" t ] in
  let m = module_ "WithQRef" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test action generation. *)
let test_action () =
  let act = on_success [ assign "ptr" (int 42); return_bool true_ ] in
  let s = struct_ "WithAction" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithAction" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test on_act action. *)
let test_on_act () =
  let act = on_act [ assign "ptr" (int 0) ] in
  let s = struct_ "WithOnAct" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithOnAct" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test abort action. *)
let test_abort () =
  let act = on_success [ abort ] in
  let s = struct_ "WithAbort" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithAbort" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test action_if. *)
let test_action_if () =
  let stmt =
    action_if
      Expr.(ref "x" > int 10)
      [ assign "ptr" (int 1) ]
      (Some [ assign "ptr" (int 0) ])
  in
  let act = on_success [ stmt ] in
  let s = struct_ "WithIf" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithIf" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test var action statement. *)
let test_var () =
  let act = on_success [ var "tmp" (int 42); assign "ptr" (ref "tmp") ] in
  let s = struct_ "WithVar" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithVar" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test define declaration. *)
let test_define () =
  let m =
    module_ "WithDefine"
      [ define "MAX_SIZE" 1024; typedef (struct_ "S" [ field "x" uint8 ]) ]
  in
  let _ = to_3d m in
  ()

(** Test extern_fn declaration. *)
let test_extern_fn () =
  let m =
    module_ "WithExtern"
      [
        extern_fn "validate" [ param "len" uint32 ] uint8;
        typedef (struct_ "S" [ field "x" uint8 ]);
      ]
  in
  let _ = to_3d m in
  ()

(** Test extern_probe declaration. *)
let test_extern_probe () =
  let m =
    module_ "WithProbe"
      [
        extern_probe "my_probe";
        extern_probe ~init:true "my_init_probe";
        typedef (struct_ "S" [ field "x" uint8 ]);
      ]
  in
  let _ = to_3d m in
  ()

(** Test complex nested structure. *)
let test_complex_nested () =
  let inner = struct_ "Inner" [ field "a" uint8 ] in
  let outer =
    struct_ "Outer" [ field "i" (struct_typ inner); field "b" uint16 ]
  in
  let m = module_ "Nested" [ typedef inner; typedef outer ] in
  let _ = to_3d m in
  ()

(** Test big-endian struct with all BE types. *)
let test_be_struct () =
  let s =
    struct_ "BigEndian"
      [
        field "a" uint16be;
        field "b" uint32be;
        field "c" uint64be;
        field "d" uint63be;
      ]
  in
  let m = module_ "BigEndian" [ typedef s ] in
  let _ = to_3d m in
  ()

(** {1 Parsing Tests} *)

let test_parse_uint8 buf =
  let buf = truncate buf in
  let _ = parse_string uint8 buf in
  ()

let test_parse_uint16 buf =
  let buf = truncate buf in
  let _ = parse_string uint16 buf in
  ()

let test_parse_uint16be buf =
  let buf = truncate buf in
  let _ = parse_string uint16be buf in
  ()

let test_parse_uint32 buf =
  let buf = truncate buf in
  let _ = parse_string uint32 buf in
  ()

let test_parse_uint32be buf =
  let buf = truncate buf in
  let _ = parse_string uint32be buf in
  ()

let test_parse_uint63 buf =
  let buf = truncate buf in
  let _ = parse_string uint63 buf in
  ()

let test_parse_uint63be buf =
  let buf = truncate buf in
  let _ = parse_string uint63be buf in
  ()

let test_parse_uint64 buf =
  let buf = truncate buf in
  let _ = parse_string uint64 buf in
  ()

let test_parse_uint64be buf =
  let buf = truncate buf in
  let _ = parse_string uint64be buf in
  ()

let test_parse_bitfield buf =
  let buf = truncate buf in
  let t = bits ~width:6 bf_uint32 in
  let _ = parse_string t buf in
  ()

let test_parse_bf_uint8 buf =
  let buf = truncate buf in
  let t = bits ~width:3 bf_uint8 in
  let _ = parse_string t buf in
  ()

let test_parse_bf_uint16 buf =
  let buf = truncate buf in
  let t = bits ~width:10 bf_uint16 in
  let _ = parse_string t buf in
  ()

let test_parse_bf_uint16be buf =
  let buf = truncate buf in
  let t = bits ~width:10 bf_uint16be in
  let _ = parse_string t buf in
  ()

let test_parse_bf_uint32be buf =
  let buf = truncate buf in
  let t = bits ~width:20 bf_uint32be in
  let _ = parse_string t buf in
  ()

let test_parse_map buf =
  let buf = truncate buf in
  let t = map (fun n -> n * 2) (fun n -> n / 2) uint8 in
  let _ = parse_string t buf in
  ()

let test_parse_bool buf =
  let buf = truncate buf in
  let t = bool uint8 in
  let _ = parse_string t buf in
  ()

let test_parse_unit buf =
  let buf = truncate buf in
  let _ = parse_string unit buf in
  ()

let test_parse_array buf =
  let buf = truncate buf in
  let len = min 10 (String.length buf) in
  let arr = array ~len:(int len) uint8 in
  let _ = parse_string arr buf in
  ()

let test_parse_byte_array buf =
  let buf = truncate buf in
  let size = min 10 (String.length buf) in
  let ba = byte_array ~size:(int size) in
  let _ = parse_string ba buf in
  ()

let test_parse_enum buf =
  let buf = truncate buf in
  let e = enum "TestEnum" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let _ = parse_string e buf in
  ()

let test_parse_where buf =
  let buf = truncate buf in
  let t = where true_ uint8 in
  let _ = parse_string t buf in
  ()

let test_parse_all_bytes buf =
  let buf = truncate buf in
  let _ = parse_string all_bytes buf in
  ()

let test_parse_all_zeros buf =
  let buf = truncate buf in
  let _ = parse_string all_zeros buf in
  ()

let test_parse_struct buf =
  let buf = truncate buf in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint16; field "c" uint32 ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

let test_parse_struct_constrained buf =
  let buf = truncate buf in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

let test_parse_struct_be buf =
  let buf = truncate buf in
  let s =
    struct_ "BE" [ field "a" uint16be; field "b" uint32be; field "c" uint64be ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

let test_parse_struct_bitfields buf =
  let buf = truncate buf in
  let s =
    struct_ "BF"
      [
        field "a" (bits ~width:3 bf_uint8);
        field "b" (bits ~width:5 bf_uint8);
        field "c" (bits ~width:10 bf_uint16);
        field "d" (bits ~width:6 bf_uint16);
      ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

let test_parse_anon_field buf =
  let buf = truncate buf in
  let s =
    struct_ "Anon" [ field "x" uint8; anon_field uint8; field "y" uint16 ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

let test_parse_casetype buf =
  let buf = truncate buf in
  let t =
    casetype "Tag" uint8 [ case 0 uint8; case 1 uint16; default uint32 ]
  in
  let _ = parse_string t buf in
  ()

(** {1 Roundtrip Tests} *)

let test_roundtrip_uint8 n =
  let n = abs n mod 256 in
  let encoded = encode_to_string uint8 n in
  match parse_string uint8 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint8 roundtrip mismatch"
  | Error _ -> Cr.fail "uint8 roundtrip parse failed"

let test_roundtrip_uint16 n =
  let n = abs n mod 65536 in
  let encoded = encode_to_string uint16 n in
  match parse_string uint16 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint16 roundtrip mismatch"
  | Error _ -> Cr.fail "uint16 roundtrip parse failed"

let test_roundtrip_uint16be n =
  let n = abs n mod 65536 in
  let encoded = encode_to_string uint16be n in
  match parse_string uint16be encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint16be roundtrip mismatch"
  | Error _ -> Cr.fail "uint16be roundtrip parse failed"

let test_roundtrip_uint32 n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = encode_to_string uint32 n in
  match parse_string uint32 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint32 roundtrip mismatch"
  | Error _ -> Cr.fail "uint32 roundtrip parse failed"

let test_roundtrip_uint32be n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = encode_to_string uint32be n in
  match parse_string uint32be encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint32be roundtrip mismatch"
  | Error _ -> Cr.fail "uint32be roundtrip parse failed"

let test_roundtrip_uint63 n =
  let n = abs n in
  let encoded = encode_to_string uint63 n in
  match parse_string uint63 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint63 roundtrip mismatch"
  | Error _ -> Cr.fail "uint63 roundtrip parse failed"

let test_roundtrip_uint63be n =
  let n = abs n in
  let encoded = encode_to_string uint63be n in
  match parse_string uint63be encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint63be roundtrip mismatch"
  | Error _ -> Cr.fail "uint63be roundtrip parse failed"

let test_roundtrip_uint64 n =
  let encoded = encode_to_string uint64 n in
  match parse_string uint64 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint64 roundtrip mismatch"
  | Error _ -> Cr.fail "uint64 roundtrip parse failed"

let test_roundtrip_uint64be n =
  let encoded = encode_to_string uint64be n in
  match parse_string uint64be encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint64be roundtrip mismatch"
  | Error _ -> Cr.fail "uint64be roundtrip parse failed"

let test_roundtrip_map n =
  let n = abs n mod 256 in
  let t = map (fun x -> x * 2) (fun x -> x / 2) uint8 in
  let encoded = encode_to_string t (n * 2) in
  match parse_string t encoded with
  | Ok decoded -> if n * 2 <> decoded then Cr.fail "map roundtrip mismatch"
  | Error _ -> Cr.fail "map roundtrip parse failed"

let test_roundtrip_bool n =
  let v = n mod 2 = 0 in
  let t = bool uint8 in
  let encoded = encode_to_string t v in
  match parse_string t encoded with
  | Ok decoded -> if v <> decoded then Cr.fail "bool roundtrip mismatch"
  | Error _ -> Cr.fail "bool roundtrip parse failed"

let test_roundtrip_array a b c =
  let arr = [ abs a mod 256; abs b mod 256; abs c mod 256 ] in
  let t = array ~len:(int 3) uint8 in
  let encoded = encode_to_string t arr in
  match parse_string t encoded with
  | Ok decoded -> if arr <> decoded then Cr.fail "array roundtrip mismatch"
  | Error _ -> Cr.fail "array roundtrip parse failed"

let test_roundtrip_byte_array buf =
  let buf = truncate buf in
  let len = String.length buf in
  if len > 0 then begin
    let t = byte_array ~size:(int len) in
    let encoded = encode_to_string t buf in
    match parse_string t encoded with
    | Ok decoded ->
        if buf <> decoded then Cr.fail "byte_array roundtrip mismatch"
    | Error _ -> Cr.fail "byte_array roundtrip parse failed"
  end

let test_roundtrip_enum n =
  let n = abs n mod 3 in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let encoded = encode_to_string t n in
  match parse_string t encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "enum roundtrip mismatch"
  | Error _ -> Cr.fail "enum roundtrip parse failed"

(** {1 Record Codec Tests} *)

type test_record = { x : int; y : int; z : int }

let test_record_codec =
  let open Codec in
  let r, _ =
    record "TestRecord" (fun x y z -> { x; y; z })
    |+ field "x" uint8 (fun r -> r.x)
  in
  let r, _ = r |+ field "y" uint16 (fun r -> r.y) in
  let r, _ = r |+ field "z" uint32 (fun r -> r.z) in
  seal r

let test_record_roundtrip x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land 0xFFFFFFFF in
  let original = { x; y; z } in
  match encode_record_to_string test_record_codec original with
  | Error _ -> Cr.fail "record encode failed"
  | Ok encoded -> (
      match decode_record_from_string test_record_codec encoded with
      | Ok decoded ->
          if original.x <> decoded.x then Cr.fail "record x mismatch";
          if original.y <> decoded.y then Cr.fail "record y mismatch";
          if original.z <> decoded.z then Cr.fail "record z mismatch"
      | Error _ -> Cr.fail "record roundtrip decode failed")

let test_record_decode_crash buf =
  let buf = truncate buf in
  let _ = decode_record_from_string test_record_codec buf in
  ()

type be_record = { a : int; b : int }
(** Record codec with big-endian fields. *)

let be_record_codec =
  let open Codec in
  let r, _ =
    record "BERecord" (fun a b -> { a; b }) |+ field "a" uint16be (fun r -> r.a)
  in
  let r, _ = r |+ field "b" uint32be (fun r -> r.b) in
  seal r

let test_record_be_roundtrip a b =
  let a = abs a mod 65536 in
  let b = b land 0xFFFFFFFF in
  let original = { a; b } in
  match encode_record_to_string be_record_codec original with
  | Error _ -> Cr.fail "be record encode failed"
  | Ok encoded -> (
      match decode_record_from_string be_record_codec encoded with
      | Ok decoded ->
          if original.a <> decoded.a then Cr.fail "be record a mismatch";
          if original.b <> decoded.b then Cr.fail "be record b mismatch"
      | Error _ -> Cr.fail "be record roundtrip decode failed")

type bool_record = { flag : bool; value : int }
(** Record codec with bool/map fields. *)

let bool_record_codec =
  let open Codec in
  let r, _ =
    record "BoolRecord" (fun flag value -> { flag; value })
    |+ field "flag" (bool uint8) (fun r -> r.flag)
  in
  let r, _ = r |+ field "value" uint16 (fun r -> r.value) in
  seal r

let test_record_bool_roundtrip n =
  let flag = n mod 2 = 0 in
  let value = abs n mod 65536 in
  let original = { flag; value } in
  match encode_record_to_string bool_record_codec original with
  | Error _ -> Cr.fail "bool record encode failed"
  | Ok encoded -> (
      match decode_record_from_string bool_record_codec encoded with
      | Ok decoded ->
          if original.flag <> decoded.flag then
            Cr.fail "bool record flag mismatch";
          if original.value <> decoded.value then
            Cr.fail "bool record value mismatch"
      | Error _ -> Cr.fail "bool record roundtrip decode failed")

(** {1 Test Registration} *)

let pp_tests =
  [
    test_case "pp uint8" [ Cr.const () ] test_pp_uint8;
    test_case "pp uint16" [ Cr.const () ] test_pp_uint16;
    test_case "pp uint16be" [ Cr.const () ] test_pp_uint16be;
    test_case "pp uint32" [ Cr.const () ] test_pp_uint32;
    test_case "pp uint32be" [ Cr.const () ] test_pp_uint32be;
    test_case "pp uint63" [ Cr.const () ] test_pp_uint63;
    test_case "pp uint63be" [ Cr.const () ] test_pp_uint63be;
    test_case "pp uint64" [ Cr.const () ] test_pp_uint64;
    test_case "pp uint64be" [ Cr.const () ] test_pp_uint64be;
    test_case "pp bitfield" [ Cr.range 33 ] test_pp_bitfield;
    test_case "pp bf_uint8" [ Cr.int ] test_pp_bf_uint8;
    test_case "pp bf_uint16" [ Cr.int ] test_pp_bf_uint16;
    test_case "pp bf_uint16be" [ Cr.int ] test_pp_bf_uint16be;
    test_case "pp bf_uint32be" [ Cr.int ] test_pp_bf_uint32be;
    test_case "pp map" [ Cr.const () ] test_pp_map;
    test_case "pp bool" [ Cr.const () ] test_pp_bool;
    test_case "pp cases" [ Cr.const () ] test_pp_cases;
    test_case "pp unit" [ Cr.const () ] test_pp_unit;
    test_case "pp module" [ Cr.const () ] test_pp_module_simple;
  ]

let codegen_tests =
  [
    test_case "struct random fields" [ Cr.range 100 ] test_struct_random_fields;
    test_case "enum random cases" [ Cr.range 100 ] test_enum_random_cases;
    test_case "casetype random" [ Cr.range 100 ] test_casetype_random;
    test_case "casetype inline" [ Cr.const () ] test_casetype_inline;
    test_case "constraint expr" [ Cr.int ] test_constraint_expr;
    test_case "bitfield constraint" [ Cr.range 32 ] test_bitfield_constraint;
    test_case "bitwise expr" [ Cr.int ] test_bitwise_expr;
    test_case "logical expr" [ Cr.const () ] test_logical_expr;
    test_case "bitwise ops" [ Cr.const () ] test_bitwise_ops;
    test_case "logical ops" [ Cr.const () ] test_logical_ops;
    test_case "cast expr" [ Cr.const () ] test_cast_expr;
    test_case "cast variants" [ Cr.const () ] test_cast_variants;
    test_case "sizeof expr" [ Cr.const () ] test_sizeof_expr;
    test_case "array type" [ Cr.int ] test_array_type;
    test_case "byte array" [ Cr.int ] test_byte_array;
    test_case "single elem array" [ Cr.const () ] test_single_elem_array;
    test_case "single elem at most"
      [ Cr.const () ]
      test_single_elem_array_at_most;
    test_case "anon field" [ Cr.const () ] test_anon_field;
    test_case "param struct" [ Cr.range 20 ] test_param_struct;
    test_case "mutable param" [ Cr.const () ] test_mutable_param;
    test_case "apply" [ Cr.const () ] test_apply;
    test_case "type ref" [ Cr.const () ] test_type_ref;
    test_case "qualified ref" [ Cr.const () ] test_qualified_ref;
    test_case "action" [ Cr.const () ] test_action;
    test_case "on_act" [ Cr.const () ] test_on_act;
    test_case "abort" [ Cr.const () ] test_abort;
    test_case "action_if" [ Cr.const () ] test_action_if;
    test_case "var" [ Cr.const () ] test_var;
    test_case "define" [ Cr.const () ] test_define;
    test_case "extern_fn" [ Cr.const () ] test_extern_fn;
    test_case "extern_probe" [ Cr.const () ] test_extern_probe;
    test_case "complex nested" [ Cr.const () ] test_complex_nested;
    test_case "be struct" [ Cr.const () ] test_be_struct;
  ]

let parse_tests =
  [
    test_case "parse uint8" [ Cr.bytes ] test_parse_uint8;
    test_case "parse uint16" [ Cr.bytes ] test_parse_uint16;
    test_case "parse uint16be" [ Cr.bytes ] test_parse_uint16be;
    test_case "parse uint32" [ Cr.bytes ] test_parse_uint32;
    test_case "parse uint32be" [ Cr.bytes ] test_parse_uint32be;
    test_case "parse uint63" [ Cr.bytes ] test_parse_uint63;
    test_case "parse uint63be" [ Cr.bytes ] test_parse_uint63be;
    test_case "parse uint64" [ Cr.bytes ] test_parse_uint64;
    test_case "parse uint64be" [ Cr.bytes ] test_parse_uint64be;
    test_case "parse bitfield" [ Cr.bytes ] test_parse_bitfield;
    test_case "parse bf_uint8" [ Cr.bytes ] test_parse_bf_uint8;
    test_case "parse bf_uint16" [ Cr.bytes ] test_parse_bf_uint16;
    test_case "parse bf_uint16be" [ Cr.bytes ] test_parse_bf_uint16be;
    test_case "parse bf_uint32be" [ Cr.bytes ] test_parse_bf_uint32be;
    test_case "parse map" [ Cr.bytes ] test_parse_map;
    test_case "parse bool" [ Cr.bytes ] test_parse_bool;
    test_case "parse unit" [ Cr.bytes ] test_parse_unit;
    test_case "parse array" [ Cr.bytes ] test_parse_array;
    test_case "parse byte_array" [ Cr.bytes ] test_parse_byte_array;
    test_case "parse enum" [ Cr.bytes ] test_parse_enum;
    test_case "parse where" [ Cr.bytes ] test_parse_where;
    test_case "parse all_bytes" [ Cr.bytes ] test_parse_all_bytes;
    test_case "parse all_zeros" [ Cr.bytes ] test_parse_all_zeros;
    test_case "parse struct" [ Cr.bytes ] test_parse_struct;
    test_case "parse struct constrained" [ Cr.bytes ]
      test_parse_struct_constrained;
    test_case "parse struct be" [ Cr.bytes ] test_parse_struct_be;
    test_case "parse struct bitfields" [ Cr.bytes ] test_parse_struct_bitfields;
    test_case "parse anon field" [ Cr.bytes ] test_parse_anon_field;
    test_case "parse casetype" [ Cr.bytes ] test_parse_casetype;
  ]

let roundtrip_tests =
  [
    test_case "roundtrip uint8" [ Cr.int ] test_roundtrip_uint8;
    test_case "roundtrip uint16" [ Cr.int ] test_roundtrip_uint16;
    test_case "roundtrip uint16be" [ Cr.int ] test_roundtrip_uint16be;
    test_case "roundtrip uint32" [ Cr.int ] test_roundtrip_uint32;
    test_case "roundtrip uint32be" [ Cr.int ] test_roundtrip_uint32be;
    test_case "roundtrip uint63" [ Cr.int ] test_roundtrip_uint63;
    test_case "roundtrip uint63be" [ Cr.int ] test_roundtrip_uint63be;
    test_case "roundtrip uint64" [ Cr.int64 ] test_roundtrip_uint64;
    test_case "roundtrip uint64be" [ Cr.int64 ] test_roundtrip_uint64be;
    test_case "roundtrip map" [ Cr.int ] test_roundtrip_map;
    test_case "roundtrip bool" [ Cr.int ] test_roundtrip_bool;
    test_case "roundtrip array" [ Cr.int; Cr.int; Cr.int ] test_roundtrip_array;
    test_case "roundtrip byte_array" [ Cr.bytes ] test_roundtrip_byte_array;
    test_case "roundtrip enum" [ Cr.int ] test_roundtrip_enum;
  ]

let record_tests =
  [
    test_case "record roundtrip" [ Cr.int; Cr.int; Cr.int ]
      test_record_roundtrip;
    test_case "record decode crash" [ Cr.bytes ] test_record_decode_crash;
    test_case "record be roundtrip" [ Cr.int; Cr.int ] test_record_be_roundtrip;
    test_case "record bool roundtrip" [ Cr.int ] test_record_bool_roundtrip;
  ]

let suite =
  ( "wire",
    pp_tests @ codegen_tests @ parse_tests @ roundtrip_tests @ record_tests )
