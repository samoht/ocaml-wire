(* Test wire library *)

open Wire

let slice buf = Bytesrw.Bytes.Slice.make buf ~first:0 ~length:(Bytes.length buf)
let slice_off buf off len = Bytesrw.Bytes.Slice.make buf ~first:off ~length:len
let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Helper: parse from a string delivered in slices of [chunk_size] bytes.
   Forces multi-byte values to straddle slice boundaries. *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.parse typ reader

(* Helper: encode to string via streaming writer with [chunk_size] buffer *)
let encode_chunked ~chunk_size typ v =
  let buf = Buffer.create 64 in
  let writer = Bytesrw.Bytes.Writer.of_buffer ~slice_length:chunk_size buf in
  Wire.encode typ v writer;
  Bytesrw.Bytes.Writer.write_eod writer;
  Buffer.contents buf

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

let test_bitfields () =
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 bf_uint32);
        field "y"
          ~constraint_:Expr.(ref "y" <= int 900)
          (bits ~width:10 bf_uint32);
        field "z"
          ~constraint_:Expr.(ref "y" + ref "z" <= int 60000)
          (bits ~width:16 bf_uint32);
      ]
  in
  let m = module_ "Bitfields" [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_ "Enumerations"
      [
        enum_decl "Enum8"
          [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ]
          uint8;
      ]
  in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains enum" true (contains ~sub:"enum" output);
  Alcotest.(check bool) "contains Enum8_1" true (contains ~sub:"Enum8_1" output)

let test_field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ ref "a" ]) ]
  in
  let m = module_ "FieldDependence" [ typedef t_struct; typedef s_struct ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains param" true (contains ~sub:"UINT32 a" output)

let test_casetype () =
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [ decl_case 1 uint16; decl_case 2 uint32 ]
  in
  let m = module_ "Casetype" [ d_casetype ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool)
    "contains casetype" true
    (contains ~sub:"casetype" output);
  Alcotest.(check bool)
    "contains switch" true
    (contains ~sub:"switch (key)" output);
  (* Public name should not have underscore *)
  Alcotest.(check bool) "public name is D" true (contains ~sub:"} D;" output)

let test_pretty_print () =
  let simple =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint32 ]
  in
  let m = module_ "Simple" [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

(* Parsing tests *)

let test_parse_uint8 () =
  let input = "\x42" in
  match parse_string uint8 input with
  | Ok v -> Alcotest.(check int) "uint8 value" 0x42 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_le () =
  let input = "\x01\x02" in
  match parse_string uint16 input with
  | Ok v -> Alcotest.(check int) "uint16 le value" 0x0201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_be () =
  let input = "\x01\x02" in
  match parse_string uint16be input with
  | Ok v -> Alcotest.(check int) "uint16 be value" 0x0102 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_le () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32 input with
  | Ok v -> Alcotest.(check int) "uint32 le value" 0x04030201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_be () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32be input with
  | Ok v -> Alcotest.(check int) "uint32 be value" 0x01020304 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint64_le () =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match parse_string uint64 input with
  | Ok v -> Alcotest.(check int64) "uint64 le value" 0x0807060504030201L v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_array () =
  let input = "\x01\x02\x03" in
  let t = array ~len:(int 3) uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check (list int)) "array values" [ 1; 2; 3 ] v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_byte_array () =
  let input = "hello" in
  let t = byte_array ~size:(int 5) in
  match parse_string t input with
  | Ok v -> Alcotest.(check string) "byte_array value" "hello" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_enum_valid () =
  let input = "\x01" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "enum value" 1 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_enum_invalid () =
  let input = "\xFF" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok _ -> Alcotest.fail "expected error for invalid enum"
  | Error (Invalid_enum { value; _ }) ->
      Alcotest.(check int) "invalid enum value" 255 value
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_all_bytes () =
  let input = "hello world" in
  match parse_string all_bytes input with
  | Ok v -> Alcotest.(check string) "all_bytes value" "hello world" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_valid () =
  let input = "\x00\x00\x00" in
  match parse_string all_zeros input with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_invalid () =
  let input = "\x00\x01\x00" in
  match parse_string all_zeros input with
  | Ok _ -> Alcotest.fail "expected error for non-zero byte"
  | Error (All_zeros_failed { offset }) ->
      Alcotest.(check int) "non-zero offset" 1 offset
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_bitfield () =
  let input = "\xFF\xFF\xFF\xFF" in
  let t = bits ~width:6 bf_uint32 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "bitfield value (6 bits)" 63 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_eof () =
  let input = "\x01" in
  match parse_string uint16 input with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error (Unexpected_eof { expected; got }) ->
      Alcotest.(check int) "expected bytes" 2 expected;
      Alcotest.(check int) "got bytes" 1 got
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct () =
  let input = "\x01\x02\x03" in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint8; field "c" uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint () =
  (* Test struct with constraint that should pass *)
  let input = "\x0A" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint_fail () =
  (* Test struct with constraint that should fail *)
  let input = "\xFF" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* Encoding tests *)

let test_encode_uint8 () =
  let encoded = encode_to_string uint8 0x42 in
  Alcotest.(check string) "uint8 encoding" "\x42" encoded

let test_encode_uint16_le () =
  let encoded = encode_to_string uint16 0x0201 in
  Alcotest.(check string) "uint16 le encoding" "\x01\x02" encoded

let test_encode_uint16_be () =
  let encoded = encode_to_string uint16be 0x0102 in
  Alcotest.(check string) "uint16 be encoding" "\x01\x02" encoded

let test_encode_uint32_le () =
  let encoded = encode_to_string uint32 0x04030201 in
  Alcotest.(check string) "uint32 le encoding" "\x01\x02\x03\x04" encoded

let test_encode_uint32_be () =
  let encoded = encode_to_string uint32be 0x01020304 in
  Alcotest.(check string) "uint32 be encoding" "\x01\x02\x03\x04" encoded

let test_encode_array () =
  let t = array ~len:(int 3) uint8 in
  let encoded = encode_to_string t [ 1; 2; 3 ] in
  Alcotest.(check string) "array encoding" "\x01\x02\x03" encoded

let test_encode_byte_array () =
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t "hello" in
  Alcotest.(check string) "byte_array encoding" "hello" encoded

let test_encode_enum () =
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let encoded = encode_to_string t 1 in
  Alcotest.(check string) "enum encoding" "\x01" encoded

let test_encode_bitfield () =
  let t = bits ~width:6 bf_uint32 in
  let encoded = encode_to_string t 63 in
  (* 63 = 0x3F, but stored in 4 bytes as uint32 LE *)
  Alcotest.(check string) "bitfield encoding" "\x3F\x00\x00\x00" encoded

(* Roundtrip tests *)

let test_roundtrip_uint8 () =
  let original = 0x42 in
  let encoded = encode_to_string uint8 original in
  match parse_string uint8 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint8" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint16 () =
  let original = 0x1234 in
  let encoded = encode_to_string uint16 original in
  match parse_string uint16 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint16" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint32 () =
  let original = 0x12345678 in
  let encoded = encode_to_string uint32 original in
  match parse_string uint32 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint32" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_array () =
  let original = [ 1; 2; 3; 4; 5 ] in
  let t = array ~len:(int 5) uint8 in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded -> Alcotest.(check (list int)) "roundtrip array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_byte_array () =
  let original = "hello" in
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded ->
      Alcotest.(check string) "roundtrip byte_array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* Record codec tests *)

type simple_record = { a : int; b : int; c : int }

let simple_record_codec =
  let open Codec in
  let r, _ =
    record "SimpleRecord" (fun a b c -> { a; b; c })
    |+ field "a" uint8 (fun r -> r.a)
  in
  let r, _ = r |+ field "b" uint16 (fun r -> r.b) in
  let r, _ = r |+ field "c" uint32 (fun r -> r.c) in
  seal r

let test_record_encode () =
  let v = { a = 0x42; b = 0x1234; c = 0x56789ABC } in
  match encode_record_to_string simple_record_codec v with
  | Error e -> Alcotest.failf "%a" pp_parse_error e
  | Ok encoded ->
      (* uint8 + uint16_le + uint32_le *)
      Alcotest.(check int) "length" 7 (String.length encoded);
      Alcotest.(check int) "byte 0 (a)" 0x42 (Char.code encoded.[0]);
      (* uint16 LE: 0x1234 -> 0x34, 0x12 *)
      Alcotest.(check int) "byte 1 (b low)" 0x34 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2 (b high)" 0x12 (Char.code encoded.[2])

let test_record_decode () =
  let input = "\x42\x34\x12\xBC\x9A\x78\x56" in
  match decode_record_from_string simple_record_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 0x42 v.a;
      Alcotest.(check int) "b" 0x1234 v.b;
      Alcotest.(check int) "c" 0x56789ABC v.c
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = 0x12345678 } in
  match encode_record_to_string simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record_from_string simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int) "c roundtrip" original.c decoded.c
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_to_struct () =
  let s = Codec.to_struct simple_record_codec in
  let m = module_ "SimpleRecord" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a;" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b;" output);
  Alcotest.(check bool) "contains field c" true (contains ~sub:"c;" output)

(* Record with multiple uint16be fields *)
type multi_record = { x : int; y : int }

let multi_record_codec =
  let open Codec in
  let r, _ =
    record "MultiRecord" (fun x y -> { x; y })
    |+ field "x" uint16be (fun r -> r.x)
  in
  let r, _ = r |+ field "y" uint16be (fun r -> r.y) in
  seal r

let test_record_with_multi () =
  let original = { x = 0x1234; y = 0x5678 } in
  match encode_record_to_string multi_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record_from_string multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "x" original.x decoded.x;
          Alcotest.(check int) "y" original.y decoded.y
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Record with byte_array field *)
type ba_record = { id : int; uuid : string; tag : int }

let ba_record_codec =
  let open Codec in
  let r, _ =
    record "BaRecord" (fun id uuid tag -> { id; uuid; tag })
    |+ field "id" uint32be (fun r -> r.id)
  in
  let r, _ = r |+ field "uuid" (byte_array ~size:(int 16)) (fun r -> r.uuid) in
  let r, _ = r |+ field "tag" uint16be (fun r -> r.tag) in
  seal r

let test_record_byte_array_roundtrip () =
  let original = { id = 0x12345678; uuid = "0123456789abcdef"; tag = 0xABCD } in
  match encode_record_to_string ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record_from_string ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "id" original.id decoded.id;
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int) "tag" original.tag decoded.tag
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_padding () =
  (* Short string should be zero-padded *)
  let original = { id = 1; uuid = "short"; tag = 2 } in
  match encode_record_to_string ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      (* Verify zero padding: bytes 9..19 should be zero *)
      for i = 9 to 19 do
        Alcotest.(check int)
          (Fmt.str "padding byte %d" i)
          0
          (Char.code encoded.[i])
      done;
      match decode_record_from_string ba_record_codec encoded with
      | Ok decoded ->
          (* Decoded uuid includes the zero padding *)
          Alcotest.(check int) "uuid length" 16 (String.length decoded.uuid);
          Alcotest.(check string)
            "uuid prefix" "short"
            (String.sub decoded.uuid 0 5)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Codec bitfield tests *)

type bf32_record = { bf_a : int; bf_b : int; bf_c : int; bf_d : int }

let bf32_codec =
  let open Codec in
  let r, _ =
    record "Bf32Test" (fun a b c d ->
        { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
    |+ field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a)
  in
  let r, _ = r |+ field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b) in
  let r, _ = r |+ field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c) in
  let r, _ = r |+ field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d) in
  seal r

type bf16_record = {
  bf_ver : int;
  bf_flags : int;
  bf_id : int;
  bf_count : int;
  bf_len : int;
}

let bf16_codec =
  let open Codec in
  let r, _ =
    record "Bf16Test" (fun ver flags id count len ->
        {
          bf_ver = ver;
          bf_flags = flags;
          bf_id = id;
          bf_count = count;
          bf_len = len;
        })
    |+ field "ver" (bits ~width:3 bf_uint16be) (fun t -> t.bf_ver)
  in
  let r, _ =
    r |+ field "flags" (bits ~width:2 bf_uint16be) (fun t -> t.bf_flags)
  in
  let r, _ = r |+ field "id" (bits ~width:11 bf_uint16be) (fun t -> t.bf_id) in
  let r, _ =
    r |+ field "count" (bits ~width:14 bf_uint16be) (fun t -> t.bf_count)
  in
  let r, _ = r |+ field "len" (bits ~width:2 bf_uint16be) (fun t -> t.bf_len) in
  seal r

let test_codec_bitfield_wire_size () =
  Alcotest.(check int) "bf32 wire_size" 4 (Codec.wire_size bf32_codec);
  Alcotest.(check int) "bf16 wire_size" 4 (Codec.wire_size bf16_codec)

let test_codec_bitfield_roundtrip () =
  let original = { bf_a = 5; bf_b = 20; bf_c = 0x1234; bf_d = 0xAB } in
  match encode_record_to_string bf32_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record_from_string bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" original.bf_a decoded.bf_a;
          Alcotest.(check int) "b" original.bf_b decoded.bf_b;
          Alcotest.(check int) "c" original.bf_c decoded.bf_c;
          Alcotest.(check int) "d" original.bf_d decoded.bf_d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_byte_layout () =
  (* a=5 (3b), b=20 (5b), c=0x1234 (16b), d=0xAB (8b)
     MSB-first packing: 101_10100_0001001000110100_10101011
     = 0xB4 0x12 0x34 0xAB *)
  let v = { bf_a = 5; bf_b = 20; bf_c = 0x1234; bf_d = 0xAB } in
  match encode_record_to_string bf32_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded ->
      Alcotest.(check int) "length" 4 (String.length encoded);
      Alcotest.(check int) "byte 0" 0xB4 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0x12 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2" 0x34 (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xAB (Char.code encoded.[3])

let test_codec_bitfield_decode () =
  (* Decode 0xB41234AB -> a=5, b=20, c=0x1234, d=0xAB *)
  let input = "\xB4\x12\x34\xAB" in
  match decode_record_from_string bf32_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 5 v.bf_a;
      Alcotest.(check int) "b" 20 v.bf_b;
      Alcotest.(check int) "c" 0x1234 v.bf_c;
      Alcotest.(check int) "d" 0xAB v.bf_d
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_bitfield_multi_group () =
  (* Two bf_uint16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
  let v =
    { bf_ver = 5; bf_flags = 2; bf_id = 0x7FF; bf_count = 0x3FFF; bf_len = 3 }
  in
  match encode_record_to_string bf16_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      (* First group: 101_10_11111111111 = 0xB7FF *)
      Alcotest.(check int) "byte 0" 0xB7 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0xFF (Char.code encoded.[1]);
      (* Second group: 11111111111111_11 = 0xFFFF *)
      Alcotest.(check int) "byte 2" 0xFF (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xFF (Char.code encoded.[3]);
      (* Roundtrip decode *)
      match decode_record_from_string bf16_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "ver" v.bf_ver decoded.bf_ver;
          Alcotest.(check int) "flags" v.bf_flags decoded.bf_flags;
          Alcotest.(check int) "id" v.bf_id decoded.bf_id;
          Alcotest.(check int) "count" v.bf_count decoded.bf_count;
          Alcotest.(check int) "len" v.bf_len decoded.bf_len
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_to_struct () =
  let s = Codec.to_struct bf32_codec in
  let m = module_ "Bf32Test" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

(* Zero-copy view tests *)

let test_view_get_uint () =
  let codec, f_x, f_y =
    let open Codec in
    let r, f_x =
      record "ViewUint" (fun a b -> { x = a; y = b })
      |+ field "x" uint16be (fun r -> r.x)
    in
    let r, f_y = r |+ field "y" uint16be (fun r -> r.y) in
    (seal r, f_x, f_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  let s = slice buf in
  Alcotest.(check int) "get x" 0x1234 (Codec.get codec f_x s);
  Alcotest.(check int) "get y" 0x5678 (Codec.get codec f_y s)

let test_view_get_bitfield () =
  let codec, f_a, f_d =
    let open Codec in
    let r, f_a =
      record "ViewBf" (fun a b c d ->
          { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
      |+ field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a)
    in
    let r, _ = r |+ field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b) in
    let r, _ = r |+ field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c) in
    let r, f_d = r |+ field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d) in
    (seal r, f_a, f_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  let s = slice buf in
  Alcotest.(check int) "get a" 5 (Codec.get codec f_a s);
  Alcotest.(check int) "get d" 0xAB (Codec.get codec f_d s)

let test_view_get_bool () =
  let codec, f_flag =
    let open Codec in
    let r, f_flag =
      record "ViewBool" (fun flag code -> (flag, code))
      |+ field "flag" (bool (bits ~width:1 bf_uint8)) fst
    in
    let r, _ = r |+ field "code" (bits ~width:7 bf_uint8) snd in
    (seal r, f_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x80;
  Alcotest.(check bool)
    "get flag=true" true
    (Codec.get codec f_flag (slice buf));
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool)
    "get flag=false" false
    (Codec.get codec f_flag (slice buf))

let test_view_set_bitfield () =
  let codec, f_a, f_d =
    let open Codec in
    let r, f_a =
      record "ViewSetBf" (fun a b c d ->
          { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
      |+ field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a)
    in
    let r, _ = r |+ field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b) in
    let r, _ = r |+ field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c) in
    let r, f_d = r |+ field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d) in
    (seal r, f_a, f_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  let s = slice buf in
  Codec.set codec f_a s 3;
  Alcotest.(check int) "get a after set" 3 (Codec.get codec f_a s);
  let r = Codec.decode codec buf 0 in
  Alcotest.(check int) "b preserved" 20 r.bf_b;
  Alcotest.(check int) "c preserved" 0x1234 r.bf_c;
  Alcotest.(check int) "d preserved" 0xAB r.bf_d;
  Codec.set codec f_d s 0x42;
  Alcotest.(check int) "get d after set" 0x42 (Codec.get codec f_d s);
  let r = Codec.decode codec buf 0 in
  Alcotest.(check int) "a still 3" 3 r.bf_a;
  Alcotest.(check int) "b still 20" 20 r.bf_b;
  Alcotest.(check int) "c still 0x1234" 0x1234 r.bf_c

let test_view_set_uint () =
  let codec, f_x, f_y =
    let open Codec in
    let r, f_x =
      record "ViewSetUint" (fun x y -> { x; y })
      |+ field "x" uint16be (fun r -> r.x)
    in
    let r, f_y = r |+ field "y" uint16be (fun r -> r.y) in
    (seal r, f_x, f_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  let s = slice buf in
  Codec.set codec f_x s 0xAAAA;
  Alcotest.(check int) "get x after set" 0xAAAA (Codec.get codec f_x s);
  Alcotest.(check int) "y unchanged" 0x5678 (Codec.get codec f_y s)

let test_view_bounds_check () =
  let codec, f =
    let open Codec in
    let r, f =
      record "ViewBounds" (fun a -> a) |+ field "a" uint32be (fun a -> a)
    in
    (seal r, f)
  in
  let buf = Bytes.create 2 in
  match Codec.get codec f (slice buf) with
  | _ -> Alcotest.fail "expected Parse_error for short buffer"
  | exception Parse_error (Unexpected_eof _) -> ()
  | exception e -> Alcotest.failf "wrong exception: %s" (Printexc.to_string e)

let test_view_with_offset () =
  let codec, f_a =
    let open Codec in
    let r, f_a =
      record "ViewOff" (fun a -> a) |+ field "a" uint16be (fun a -> a)
    in
    (seal r, f_a)
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_uint16_be buf 2 0x2222;
  Bytes.set_uint16_be buf 4 0x3333;
  let ws = Codec.wire_size codec in
  Alcotest.(check int)
    "get at offset 2" 0x2222
    (Codec.get codec f_a (slice_off buf 2 ws))

let test_view_set_bool () =
  let codec, f_flag =
    let open Codec in
    let r, f_flag =
      record "ViewSetBool" (fun flag code -> (flag, code))
      |+ field "flag" (bool (bits ~width:1 bf_uint8)) fst
    in
    let r, _ = r |+ field "code" (bits ~width:7 bf_uint8) snd in
    (seal r, f_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  let s = slice buf in
  Codec.set codec f_flag s true;
  Alcotest.(check bool)
    "get flag after set true" true (Codec.get codec f_flag s);
  Alcotest.(check int) "byte value" 0x80 (Bytes.get_uint8 buf 0);
  Codec.set codec f_flag s false;
  Alcotest.(check bool)
    "get flag after set false" false (Codec.get codec f_flag s);
  Alcotest.(check int) "byte cleared" 0x00 (Bytes.get_uint8 buf 0)

(* Field sharing tests — same field spec used in two codecs *)

let test_view_shared_field_spec () =
  (* Same field spec added to two codecs with different layouts.
     Codec1: [u16be x] [u16be y]   → x at offset 0
     Codec2: [u16be pad] [u16be x] → x at offset 2
     The configured fields must be independent. *)
  let spec = Codec.field "x" uint16be (fun (x, _) -> x) in
  let codec1, f1_x =
    let open Codec in
    let r, f_x = record "Share1" (fun x y -> (x, y)) |+ spec in
    let r, _ = r |+ field "y" uint16be (fun (_, y) -> y) in
    (seal r, f_x)
  in
  let codec2, f2_x =
    let open Codec in
    let r, _ =
      record "Share2" (fun _pad x -> (x, 0))
      |+ field "pad" uint16be (fun _ -> 0)
    in
    let r, f_x = r |+ spec in
    (seal r, f_x)
  in
  let buf1 = Bytes.create 4 in
  Bytes.set_uint16_be buf1 0 0xAAAA;
  Bytes.set_uint16_be buf1 2 0xBBBB;
  let buf2 = Bytes.create 4 in
  Bytes.set_uint16_be buf2 0 0x0000;
  Bytes.set_uint16_be buf2 2 0xCCCC;
  let s1 = slice buf1 in
  let s2 = slice buf2 in
  (* f1_x reads at offset 0, f2_x reads at offset 2 *)
  Alcotest.(check int) "codec1 get x" 0xAAAA (Codec.get codec1 f1_x s1);
  Alcotest.(check int) "codec2 get x" 0xCCCC (Codec.get codec2 f2_x s2)

let test_view_shared_bitfield_spec () =
  (* Same bitfield spec added to two codecs.
     Codec1: [3-bit a] [5-bit b]            → a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]           → a is bottom 3 bits
     The configured fields must read different bit positions. *)
  let spec = Codec.field "a" (bits ~width:3 bf_uint8) (fun (a, _) -> a) in
  let codec1, f1_a =
    let open Codec in
    let r, f_a = record "ShareBf1" (fun a b -> (a, b)) |+ spec in
    let r, _ = r |+ field "b" (bits ~width:5 bf_uint8) (fun (_, b) -> b) in
    (seal r, f_a)
  in
  let codec2, f2_a =
    let open Codec in
    let r, _ =
      record "ShareBf2" (fun _pad a -> (a, 0))
      |+ field "pad" (bits ~width:5 bf_uint8) (fun _ -> 0)
    in
    let r, f_a = r |+ spec in
    (seal r, f_a)
  in
  (* 0xE3 = 0b_111_00011
     codec1 reads top 3 bits → 7
     codec2 reads bottom 3 bits → 3 *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xE3;
  let s = slice buf in
  Alcotest.(check int) "codec1 get a (top 3)" 7 (Codec.get codec1 f1_a s);
  Alcotest.(check int) "codec2 get a (bot 3)" 3 (Codec.get codec2 f2_a s)

let test_view_shared_set_independent () =
  (* set via one codec's field must not affect the other's interpretation *)
  let spec = Codec.field "v" (bits ~width:4 bf_uint8) (fun (v, _) -> v) in
  let codec1, f1 =
    let open Codec in
    let r, f = record "SetShare1" (fun v pad -> (v, pad)) |+ spec in
    let r, _ = r |+ field "pad" (bits ~width:4 bf_uint8) (fun (_, p) -> p) in
    (seal r, f)
  in
  let codec2, f2 =
    let open Codec in
    let r, _ =
      record "SetShare2" (fun pad v -> (v, pad))
      |+ field "pad" (bits ~width:4 bf_uint8) (fun (_, p) -> p)
    in
    let r, f = r |+ spec in
    (seal r, f)
  in
  (* Start: 0x00. Set codec1's field (top nibble) to 0xA *)
  let buf = Bytes.create 1 in
  let s = slice buf in
  Codec.set codec1 f1 s 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* codec2's field is bottom nibble — should still be 0 *)
  Alcotest.(check int) "codec2 get after set1" 0 (Codec.get codec2 f2 s);
  (* Set codec2's field (bottom nibble) to 0x5 *)
  Codec.set codec2 f2 s 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* codec1's field should still be 0xA *)
  Alcotest.(check int) "codec1 get after set2" 0xA (Codec.get codec1 f1 s)

(* ── byte_slice tests ── *)

module Bs = Bytesrw.Bytes.Slice

let test_view_byte_slice_get () =
  (* A record with a fixed-size byte_slice field returns a sub-slice *)
  let open Codec in
  let r, _ =
    record "SliceRec" (fun hdr payload -> (hdr, payload))
    |+ field "hdr" uint16be (fun (h, _) -> h)
  in
  let r, f_payload =
    r |+ field "payload" (byte_slice ~size:(int 4)) (fun (_, p) -> p)
  in
  let codec = seal r in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xABCD;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let s = slice buf in
  let payload = Codec.get codec f_payload s in
  (* payload should be a slice into buf at offset 2, length 4 *)
  Alcotest.(check int) "payload first" 2 (Bs.first payload);
  Alcotest.(check int) "payload length" 4 (Bs.length payload);
  Alcotest.(check bool) "same buffer" true (Bs.bytes payload == buf);
  Alcotest.(check int)
    "payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload));
  Alcotest.(check int)
    "payload[3]" 0x40
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload + 3))

let test_view_byte_slice_decode () =
  (* decode also produces a correct sub-slice *)
  let open Codec in
  let r, _ =
    record "SliceDec" (fun tag payload -> (tag, payload))
    |+ field "tag" uint8 (fun (t, _) -> t)
  in
  let r, _ = r |+ field "data" (byte_slice ~size:(int 3)) (fun (_, p) -> p) in
  let codec = seal r in
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xFF;
  Bytes.set_uint8 buf 1 0xAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let tag, payload = Codec.decode codec buf 0 in
  Alcotest.(check int) "tag" 0xFF tag;
  Alcotest.(check int) "payload first" 1 (Bs.first payload);
  Alcotest.(check int) "payload length" 3 (Bs.length payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_view_byte_slice_nested () =
  (* Two-layer nested protocol: get payload slice, then get inner field *)
  let open Codec in
  let inner_r, f_val =
    record "Inner" (fun v -> v) |+ field "val" uint16be (fun v -> v)
  in
  let inner_codec = seal inner_r in
  let r, _ =
    record "Outer" (fun hdr payload -> (hdr, payload))
    |+ field "hdr" uint16be (fun (h, _) -> h)
  in
  let r, f_payload =
    r |+ field "payload" (byte_slice ~size:(int 2)) (fun (_, p) -> p)
  in
  let outer_codec = seal r in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x1234;
  let s = slice buf in
  let payload = Codec.get outer_codec f_payload s in
  let inner_val = Codec.get inner_codec f_val payload in
  Alcotest.(check int) "inner val via zero-copy" 0x1234 inner_val

(* ── Raw access: get_raw / set_raw / sub ── *)

let test_raw_get_uint () =
  let open Codec in
  let r, f_a =
    record "RawU" (fun a b -> (a, b)) |+ field "a" uint16be (fun (a, _) -> a)
  in
  let r, f_b = r |+ field "b" uint8 (fun (_, b) -> b) in
  let codec = seal r in
  let buf = Bytes.create 3 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint8 buf 2 0xFF;
  Alcotest.(check int) "get_raw a" 0x1234 (Codec.get_raw codec f_a buf 0);
  Alcotest.(check int) "get_raw b" 0xFF (Codec.get_raw codec f_b buf 0)

let test_raw_get_bitfield () =
  let open Codec in
  let r, f_hi =
    record "RawBF" (fun hi lo -> (hi, lo))
    |+ field "hi" (bits ~width:4 bf_uint8) (fun (h, _) -> h)
  in
  let r, f_lo = r |+ field "lo" (bits ~width:4 bf_uint8) (fun (_, l) -> l) in
  let codec = seal r in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  (* hi=0xA=10, lo=0x7=7 *)
  Alcotest.(check int) "get_raw hi" 0xA (Codec.get_raw codec f_hi buf 0);
  Alcotest.(check int) "get_raw lo" 0x7 (Codec.get_raw codec f_lo buf 0)

let test_raw_set_uint () =
  let open Codec in
  let r, f_a =
    record "RawSU" (fun a b -> (a, b)) |+ field "a" uint16be (fun (a, _) -> a)
  in
  let r, f_b = r |+ field "b" uint8 (fun (_, b) -> b) in
  let codec = seal r in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  Codec.set_raw codec f_a buf 0 0xABCD;
  Codec.set_raw codec f_b buf 0 0x42;
  Alcotest.(check int) "set_raw a" 0xABCD (Bytes.get_uint16_be buf 0);
  Alcotest.(check int) "set_raw b" 0x42 (Bytes.get_uint8 buf 2)

let test_raw_set_bitfield () =
  let open Codec in
  let r, f_hi =
    record "RawSBF" (fun hi lo -> (hi, lo))
    |+ field "hi" (bits ~width:4 bf_uint8) (fun (h, _) -> h)
  in
  let r, f_lo = r |+ field "lo" (bits ~width:4 bf_uint8) (fun (_, l) -> l) in
  let codec = seal r in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  Codec.set_raw codec f_hi buf 0 0xC;
  Codec.set_raw codec f_lo buf 0 0x3;
  Alcotest.(check int) "set_raw bf byte" 0xC3 (Bytes.get_uint8 buf 0)

let test_raw_sub_nested () =
  (* Two-layer nested protocol using sub + get_raw: zero alloc *)
  let open Codec in
  let inner_r, f_val =
    record "Inner" (fun v -> v) |+ field "val" uint16be (fun v -> v)
  in
  let inner_codec = seal inner_r in
  let r, _ =
    record "Outer" (fun hdr payload -> (hdr, payload))
    |+ field "hdr" uint16be (fun (h, _) -> h)
  in
  let r, f_payload =
    r |+ field "payload" (byte_slice ~size:(int 2)) (fun (_, p) -> p)
  in
  let outer_codec = seal r in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x5678;
  let inner_off = Codec.sub outer_codec f_payload buf 0 in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val = Codec.get_raw inner_codec f_val buf inner_off in
  Alcotest.(check int) "inner val via sub+get_raw" 0x5678 inner_val

let test_raw_sub_three_layers () =
  (* Three-layer: outer -> mid -> inner, all zero-alloc via sub+get_raw *)
  let open Codec in
  let inner_r, f_x = record "L3" (fun x -> x) |+ field "x" uint8 (fun x -> x) in
  let inner = seal inner_r in
  let mid_r, _ =
    record "L2" (fun tag payload -> (tag, payload))
    |+ field "tag" uint8 (fun (t, _) -> t)
  in
  let mid_r, f_mid_payload =
    mid_r |+ field "data" (byte_slice ~size:(int 1)) (fun (_, p) -> p)
  in
  let mid = seal mid_r in
  let outer_r, _ =
    record "L1" (fun hdr body -> (hdr, body))
    |+ field "hdr" uint16be (fun (h, _) -> h)
  in
  let outer_r, f_body =
    outer_r |+ field "body" (byte_slice ~size:(int 2)) (fun (_, b) -> b)
  in
  let outer = seal outer_r in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let mid_off = Codec.sub outer f_body buf 0 in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off = Codec.sub mid f_mid_payload buf mid_off in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = Codec.get_raw inner f_x buf inner_off in
  Alcotest.(check int) "3-layer get_raw" 0xCC x

let test_raw_with_offset () =
  (* get_raw / set_raw work correctly with non-zero base offset *)
  let open Codec in
  let r, f_v =
    record "RawOff" (fun v -> v) |+ field "v" uint32be (fun v -> v)
  in
  let codec = seal r in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  Codec.set_raw codec f_v buf 10 0xDEADBEEF;
  Alcotest.(check int)
    "get_raw at offset 10" 0xDEADBEEF
    (Codec.get_raw codec f_v buf 10)

(* FFI stub generation tests *)

let test_c_stubs () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let stubs = to_c_stubs [ s ] in
  Alcotest.(check bool)
    "contains check stub" true
    (contains ~sub:"caml_wire_simpleheader_check" stubs);
  Alcotest.(check bool)
    "contains error handler" true
    (contains ~sub:"simpleheader_err" stubs)

(* ── Streaming: cross-slice boundary tests ── *)

(* Parse roundtrip with every chunk size forcing boundary straddles *)
let test_stream_uint8 () =
  let encoded = encode_to_string uint8 42 in
  match parse_chunked ~chunk_size:1 uint8 encoded with
  | Ok v -> Alcotest.(check int) "uint8 chunk=1" 42 v
  | Error e -> Alcotest.failf "uint8 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk1 () =
  let encoded = encode_to_string uint16 0xCAFE in
  match parse_chunked ~chunk_size:1 uint16 encoded with
  | Ok v -> Alcotest.(check int) "uint16 chunk=1" 0xCAFE v
  | Error e -> Alcotest.failf "uint16 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk3 () =
  (* chunk=3 means 2-byte value fits in one slice — fast path *)
  let encoded = encode_to_string uint16be 0xBEEF in
  match parse_chunked ~chunk_size:3 uint16be encoded with
  | Ok v -> Alcotest.(check int) "uint16be chunk=3" 0xBEEF v
  | Error e -> Alcotest.failf "uint16be chunk=3: %a" pp_parse_error e

let test_stream_uint32_chunk1 () =
  let encoded = encode_to_string uint32 0xDEADBEEF in
  match parse_chunked ~chunk_size:1 uint32 encoded with
  | Ok v -> Alcotest.(check int) "uint32 chunk=1" 0xDEADBEEF v
  | Error e -> Alcotest.failf "uint32 chunk=1: %a" pp_parse_error e

let test_stream_uint32_chunk3 () =
  (* chunk=3: 4-byte value straddles at byte 3 *)
  let encoded = encode_to_string uint32be 0x12345678 in
  match parse_chunked ~chunk_size:3 uint32be encoded with
  | Ok v -> Alcotest.(check int) "uint32be chunk=3" 0x12345678 v
  | Error e -> Alcotest.failf "uint32be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk1 () =
  let encoded = encode_to_string uint64 0x0102030405060708L in
  match parse_chunked ~chunk_size:1 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=1" 0x0102030405060708L v
  | Error e -> Alcotest.failf "uint64 chunk=1: %a" pp_parse_error e

let test_stream_uint64_chunk3 () =
  let encoded = encode_to_string uint64be 0xFEDCBA9876543210L in
  match parse_chunked ~chunk_size:3 uint64be encoded with
  | Ok v -> Alcotest.(check int64) "uint64be chunk=3" 0xFEDCBA9876543210L v
  | Error e -> Alcotest.failf "uint64be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk5 () =
  let encoded = encode_to_string uint64 0xAAAABBBBCCCCDDDDL in
  match parse_chunked ~chunk_size:5 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=5" 0xAAAABBBBCCCCDDDDL v
  | Error e -> Alcotest.failf "uint64 chunk=5: %a" pp_parse_error e

let test_stream_bitfield_chunk1 () =
  (* Bitfield: 6+10+16 bits packed in a uint32 *)
  let bf = bits ~width:6 bf_uint32 in
  let encoded = encode_to_string bf 42 in
  match parse_chunked ~chunk_size:1 bf encoded with
  | Ok v ->
      (* 42 written in top 6 bits of uint32: 42 << 26, then read back as 6-bit *)
      Alcotest.(check int) "bitfield chunk=1" 42 v
  | Error e -> Alcotest.failf "bitfield chunk=1: %a" pp_parse_error e

(* Encode roundtrip through chunked writer *)
let test_stream_encode_chunk1 () =
  let v = 0xDEADBEEF in
  let encoded = encode_chunked ~chunk_size:1 uint32be v in
  match parse_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=1" v decoded
  | Error e -> Alcotest.failf "encode chunk=1: %a" pp_parse_error e

let test_stream_encode_chunk3 () =
  let v = 0x12345678 in
  let encoded = encode_chunked ~chunk_size:3 uint32be v in
  match parse_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=3" v decoded
  | Error e -> Alcotest.failf "encode chunk=3: %a" pp_parse_error e

let suite =
  ( "wire",
    [
      (* generation *)
      Alcotest.test_case "generation: bitfields" `Quick test_bitfields;
      Alcotest.test_case "generation: enumerations" `Quick test_enumerations;
      Alcotest.test_case "generation: field dependence" `Quick
        test_field_dependence;
      Alcotest.test_case "generation: casetype" `Quick test_casetype;
      Alcotest.test_case "generation: pretty print" `Quick test_pretty_print;
      (* parsing *)
      Alcotest.test_case "parse: uint8" `Quick test_parse_uint8;
      Alcotest.test_case "parse: uint16 le" `Quick test_parse_uint16_le;
      Alcotest.test_case "parse: uint16 be" `Quick test_parse_uint16_be;
      Alcotest.test_case "parse: uint32 le" `Quick test_parse_uint32_le;
      Alcotest.test_case "parse: uint32 be" `Quick test_parse_uint32_be;
      Alcotest.test_case "parse: uint64 le" `Quick test_parse_uint64_le;
      Alcotest.test_case "parse: array" `Quick test_parse_array;
      Alcotest.test_case "parse: byte_array" `Quick test_parse_byte_array;
      Alcotest.test_case "parse: enum valid" `Quick test_parse_enum_valid;
      Alcotest.test_case "parse: enum invalid" `Quick test_parse_enum_invalid;
      Alcotest.test_case "parse: all_bytes" `Quick test_parse_all_bytes;
      Alcotest.test_case "parse: all_zeros valid" `Quick
        test_parse_all_zeros_valid;
      Alcotest.test_case "parse: all_zeros invalid" `Quick
        test_parse_all_zeros_invalid;
      Alcotest.test_case "parse: bitfield" `Quick test_parse_bitfield;
      Alcotest.test_case "parse: eof error" `Quick test_parse_eof;
      Alcotest.test_case "parse: struct" `Quick test_parse_struct;
      Alcotest.test_case "parse: struct constraint" `Quick
        test_parse_struct_constraint;
      Alcotest.test_case "parse: struct constraint fail" `Quick
        test_parse_struct_constraint_fail;
      (* encoding *)
      Alcotest.test_case "encode: uint8" `Quick test_encode_uint8;
      Alcotest.test_case "encode: uint16 le" `Quick test_encode_uint16_le;
      Alcotest.test_case "encode: uint16 be" `Quick test_encode_uint16_be;
      Alcotest.test_case "encode: uint32 le" `Quick test_encode_uint32_le;
      Alcotest.test_case "encode: uint32 be" `Quick test_encode_uint32_be;
      Alcotest.test_case "encode: array" `Quick test_encode_array;
      Alcotest.test_case "encode: byte_array" `Quick test_encode_byte_array;
      Alcotest.test_case "encode: enum" `Quick test_encode_enum;
      Alcotest.test_case "encode: bitfield" `Quick test_encode_bitfield;
      (* roundtrip *)
      Alcotest.test_case "roundtrip: uint8" `Quick test_roundtrip_uint8;
      Alcotest.test_case "roundtrip: uint16" `Quick test_roundtrip_uint16;
      Alcotest.test_case "roundtrip: uint32" `Quick test_roundtrip_uint32;
      Alcotest.test_case "roundtrip: array" `Quick test_roundtrip_array;
      Alcotest.test_case "roundtrip: byte_array" `Quick
        test_roundtrip_byte_array;
      (* record *)
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
      Alcotest.test_case "record: roundtrip" `Quick test_record_roundtrip;
      Alcotest.test_case "record: to_struct" `Quick test_record_to_struct;
      Alcotest.test_case "record: with_multi" `Quick test_record_with_multi;
      Alcotest.test_case "record: byte_array roundtrip" `Quick
        test_record_byte_array_roundtrip;
      Alcotest.test_case "record: byte_array padding" `Quick
        test_record_byte_array_padding;
      (* codec bitfields *)
      Alcotest.test_case "codec bitfield: wire_size" `Quick
        test_codec_bitfield_wire_size;
      Alcotest.test_case "codec bitfield: roundtrip" `Quick
        test_codec_bitfield_roundtrip;
      Alcotest.test_case "codec bitfield: byte layout" `Quick
        test_codec_bitfield_byte_layout;
      Alcotest.test_case "codec bitfield: decode" `Quick
        test_codec_bitfield_decode;
      Alcotest.test_case "codec bitfield: multi group" `Quick
        test_codec_bitfield_multi_group;
      Alcotest.test_case "codec bitfield: to_struct" `Quick
        test_codec_bitfield_to_struct;
      (* zero-copy view *)
      Alcotest.test_case "view: get uint" `Quick test_view_get_uint;
      Alcotest.test_case "view: get bitfield" `Quick test_view_get_bitfield;
      Alcotest.test_case "view: get bool" `Quick test_view_get_bool;
      Alcotest.test_case "view: set bitfield" `Quick test_view_set_bitfield;
      Alcotest.test_case "view: set uint" `Quick test_view_set_uint;
      Alcotest.test_case "view: set bool" `Quick test_view_set_bool;
      Alcotest.test_case "view: bounds check" `Quick test_view_bounds_check;
      Alcotest.test_case "view: with offset" `Quick test_view_with_offset;
      (* field sharing *)
      Alcotest.test_case "view: shared field spec" `Quick
        test_view_shared_field_spec;
      Alcotest.test_case "view: shared bitfield spec" `Quick
        test_view_shared_bitfield_spec;
      Alcotest.test_case "view: shared set independent" `Quick
        test_view_shared_set_independent;
      (* byte_slice *)
      Alcotest.test_case "view: byte_slice get" `Quick test_view_byte_slice_get;
      Alcotest.test_case "view: byte_slice decode" `Quick
        test_view_byte_slice_decode;
      Alcotest.test_case "view: byte_slice nested" `Quick
        test_view_byte_slice_nested;
      (* raw access: get_raw / set_raw / sub *)
      Alcotest.test_case "raw: get uint" `Quick test_raw_get_uint;
      Alcotest.test_case "raw: get bitfield" `Quick test_raw_get_bitfield;
      Alcotest.test_case "raw: set uint" `Quick test_raw_set_uint;
      Alcotest.test_case "raw: set bitfield" `Quick test_raw_set_bitfield;
      Alcotest.test_case "raw: sub nested" `Quick test_raw_sub_nested;
      Alcotest.test_case "raw: sub 3 layers" `Quick test_raw_sub_three_layers;
      Alcotest.test_case "raw: with offset" `Quick test_raw_with_offset;
      (* streaming: cross-slice boundary *)
      Alcotest.test_case "stream: uint8 chunk=1" `Quick test_stream_uint8;
      Alcotest.test_case "stream: uint16 chunk=1" `Quick
        test_stream_uint16_chunk1;
      Alcotest.test_case "stream: uint16 chunk=3" `Quick
        test_stream_uint16_chunk3;
      Alcotest.test_case "stream: uint32 chunk=1" `Quick
        test_stream_uint32_chunk1;
      Alcotest.test_case "stream: uint32 chunk=3" `Quick
        test_stream_uint32_chunk3;
      Alcotest.test_case "stream: uint64 chunk=1" `Quick
        test_stream_uint64_chunk1;
      Alcotest.test_case "stream: uint64 chunk=3" `Quick
        test_stream_uint64_chunk3;
      Alcotest.test_case "stream: uint64 chunk=5" `Quick
        test_stream_uint64_chunk5;
      Alcotest.test_case "stream: bitfield chunk=1" `Quick
        test_stream_bitfield_chunk1;
      Alcotest.test_case "stream: encode chunk=1" `Quick
        test_stream_encode_chunk1;
      Alcotest.test_case "stream: encode chunk=3" `Quick
        test_stream_encode_chunk3;
      (* ffi stubs *)
      Alcotest.test_case "ffi: c_stubs" `Quick test_c_stubs;
    ] )
