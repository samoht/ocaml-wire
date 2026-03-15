(** Tests for Wire_diff — differential testing logic. *)

open Wire

let simple_codec =
  Codec.(
    record "Simple" (fun v l -> (v, l))
    |+ field "Version" uint8 fst
    |+ field "Length" uint16be snd
    |> seal)

(* Mock C functions that behave identically to OCaml *)
let mock_c_read buf =
  if String.length buf < 3 then None else Some (String.sub buf 0 3)

let mock_c_write buf = if String.length buf < 3 then None else Some buf

let mk_schema () =
  Wire_diff.schema ~name:"Simple" ~codec:simple_codec ~c_read:mock_c_read
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

let test_roundtrip_struct () =
  let s = struct_ "Simple" [ field "Version" uint8; field "Length" uint16be ] in
  let buf = "\x01\x01\x00" in
  match Wire_diff.roundtrip_struct s buf with
  | Ok rt -> Alcotest.(check string) "roundtrip" buf rt
  | Error _ -> Alcotest.fail "roundtrip failed"

let suite =
  ( "diff",
    [
      Alcotest.test_case "read match" `Quick test_read_match;
      Alcotest.test_case "read both failed" `Quick test_read_both_failed;
      Alcotest.test_case "roundtrip struct" `Quick test_roundtrip_struct;
    ] )
