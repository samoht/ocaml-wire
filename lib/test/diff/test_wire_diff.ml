(** Tests for Wire_diff — differential testing logic. *)

open Wire

let f_diff_version = Field.v "Version" uint8
let f_diff_length = Field.v "Length" uint16be

let simple_codec =
  let open Codec in
  v "Simple" (fun v l -> (v, l)) [ f_diff_version $ fst; f_diff_length $ snd ]

(* Mock C functions that behave identically to OCaml *)
let mock_c_read buf =
  if String.length buf < 3 then None else Some (String.sub buf 0 3)

let mock_c_write buf = if String.length buf < 3 then None else Some buf

let mk_schema () =
  Wire_diff.harness ~name:"Simple" ~codec:simple_codec ~c_read:mock_c_read
    ~c_write:mock_c_write ~equal:(fun (a1, a2) (b1, b2) -> a1 = b1 && a2 = b2)

let test_read_match () =
  let s = mk_schema () in
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 256;
  let result = Wire_diff.read s (Bytes.unsafe_to_string buf) in
  Alcotest.(check bool) "match" true (result = Wire_diff.Match)

let test_read_both_failed () =
  let s = mk_schema () in
  let result = Wire_diff.read s "" in
  Alcotest.(check bool) "both_failed" true (result = Wire_diff.Both_failed)

let test_write () =
  let s = mk_schema () in
  let v = (7, 1000) in
  let result = Wire_diff.write s v in
  (* mock_c_write accepts any 3-byte buffer, so a valid 3-byte encode matches *)
  Alcotest.(check bool) "write result" true (result = Wire_diff.Match)

let test_full_roundtrip () =
  let s = mk_schema () in
  let v = (3, 512) in
  let result = Wire_diff.full_roundtrip s v in
  Alcotest.(check bool) "full_roundtrip result" true (result = Wire_diff.Match)

let test_full_roundtrip_c_rejects () =
  (* C rejects OCaml-encoded bytes → Only_ocaml_ok *)
  let c_write_reject _ = None in
  let s =
    Wire_diff.harness ~name:"CReject" ~codec:simple_codec ~c_read:mock_c_read
      ~c_write:c_write_reject ~equal:(fun (a1, a2) (b1, b2) ->
        a1 = b1 && a2 = b2)
  in
  let result = Wire_diff.full_roundtrip s (3, 512) in
  Alcotest.(check bool)
    "c rejects → Only_ocaml_ok" true
    (result = Wire_diff.Only_ocaml_ok "C rejected OCaml-encoded bytes")

let test_write_c_rejects () =
  (* Same scenario in write: C rejects → Only_ocaml_ok *)
  let c_write_reject _ = None in
  let s =
    Wire_diff.harness ~name:"CRejectW" ~codec:simple_codec ~c_read:mock_c_read
      ~c_write:c_write_reject ~equal:(fun (a1, a2) (b1, b2) ->
        a1 = b1 && a2 = b2)
  in
  let result = Wire_diff.write s (3, 512) in
  Alcotest.(check bool)
    "write c rejects → Only_ocaml_ok" true
    (result = Wire_diff.Only_ocaml_ok "C rejected OCaml-encoded bytes")

let test_pack () =
  let s = mk_schema () in
  let pt = Wire_diff.pack s in
  Alcotest.(check string) "pack name" "Simple" pt.Wire_diff.name;
  Alcotest.(check int) "pack wire_size" 3 pt.Wire_diff.wire_size;
  (* test_read on a valid 3-byte buffer should match *)
  let buf = "\x01\x01\x00" in
  let result = pt.Wire_diff.test_read buf in
  Alcotest.(check bool) "pack test_read" true (result = Wire_diff.Match)

let suite =
  ( "wire_diff",
    [
      Alcotest.test_case "read match" `Quick test_read_match;
      Alcotest.test_case "read both failed" `Quick test_read_both_failed;
      Alcotest.test_case "write" `Quick test_write;
      Alcotest.test_case "full_roundtrip" `Quick test_full_roundtrip;
      Alcotest.test_case "full_roundtrip: c rejects" `Quick
        test_full_roundtrip_c_rejects;
      Alcotest.test_case "write: c rejects" `Quick test_write_c_rejects;
      Alcotest.test_case "pack" `Quick test_pack;
    ] )
