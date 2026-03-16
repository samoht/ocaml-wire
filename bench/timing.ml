(** Wire Codec benchmark: timing and allocation for decode, encode, zero-copy.

    Usage: dune exec bench/timing.exe [-- ITERATIONS] Default: 10_000_000
    iterations. *)

open Wire
open Space

(* ── Timing ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float_of_int n

(* ── Allocation ── *)

let alloc_words n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

module Bs = Bytesrw.Bytes.Slice

(* ── Codecs for micro-benchmarks ── *)

type r3 = { a : int; b : int; c : int }

let codec3, f_c =
  let open Codec in
  let r, _ =
    record "R3" (fun a b c -> { a; b; c }) |+ field "a" uint16be (fun t -> t.a)
  in
  let r, _ = r |+ field "b" uint16be (fun t -> t.b) in
  let r, f_c = r |+ field "c" uint16be (fun t -> t.c) in
  (seal r, f_c)

type r1 = { x : int }

let codec1, f_x =
  let open Codec in
  let r, f_x =
    record "R1" (fun x -> { x }) |+ field "x" uint16be (fun t -> t.x)
  in
  (seal r, f_x)

(* ── Table formatting ── *)

let table_header title cols =
  Fmt.pr "\n%s\n%s\n\n" title (String.make (String.length title) '=');
  let widths =
    List.map (fun (name, w) -> (name, max w (String.length name))) cols
  in
  List.iter (fun (name, w) -> Fmt.pr "  %-*s" w name) widths;
  Fmt.pr "\n";
  List.iter (fun (_, w) -> Fmt.pr "  %s" (String.make w '-')) widths;
  Fmt.pr "\n";
  widths

let table_row widths cells =
  List.iter2 (fun (_, w) cell -> Fmt.pr "  %-*s" w cell) widths cells;
  Fmt.pr "\n"

(* ── Main ── *)

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in

  (* ── Part 1: Timing benchmark ── *)
  let widths =
    table_header "Wire Codec Timing"
      [ ("Operation", 30); ("ns/op", 8); ("alloc", 12); ("vs decode", 8) ]
  in

  (* CLCW: 13 bitfields in 4 bytes *)
  let clcw_buf = (clcw_data 1).(0) in
  let clcw_slice = Bs.make clcw_buf ~first:0 ~length:(Bytes.length clcw_buf) in

  let clcw_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode clcw_codec clcw_buf 0)
        done)
  in
  let clcw_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode clcw_codec clcw_buf 0))
  in
  table_row widths
    [
      "CLCW decode (13 bf)";
      Fmt.str "%.1f" clcw_decode_ns;
      Fmt.str "%.0fw" clcw_decode_alloc;
      "-";
    ];

  let clcw_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get clcw_codec cw_report clcw_slice)
        done)
  in
  let clcw_get_alloc =
    alloc_words n (fun () -> ignore (Codec.get clcw_codec cw_report clcw_slice))
  in
  table_row widths
    [
      "CLCW zero-copy get";
      Fmt.str "%.1f" clcw_get_ns;
      Fmt.str "%.0fw" clcw_get_alloc;
      Fmt.str "%.0fx" (clcw_decode_ns /. clcw_get_ns);
    ];

  Fmt.pr "\n";

  (* SpacePacket: 7 fields, 3 bf_uint16be + uint16be *)
  let sp_buf = (space_packet_data 1).(0) in
  let sp_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode space_packet_codec sp_buf 0)
        done)
  in
  let sp_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode space_packet_codec sp_buf 0))
  in
  table_row widths
    [
      "SpacePacket decode (7f)";
      Fmt.str "%.1f" sp_decode_ns;
      Fmt.str "%.0fw" sp_decode_alloc;
      "-";
    ];

  (* R3: 3 uint16be fields *)
  let buf6 = Bytes.create 6 in
  let slice6 = Bs.make buf6 ~first:0 ~length:6 in

  let r3_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode codec3 buf6 0)
        done)
  in
  let r3_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get codec3 f_c slice6)
        done)
  in
  let r3_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode codec3 buf6 0))
  in
  let r3_get_alloc =
    alloc_words n (fun () -> ignore (Codec.get codec3 f_c slice6))
  in
  table_row widths
    [
      "R3 decode (3 uint16)";
      Fmt.str "%.1f" r3_decode_ns;
      Fmt.str "%.0fw" r3_decode_alloc;
      "-";
    ];
  table_row widths
    [
      "R3 zero-copy get";
      Fmt.str "%.1f" r3_get_ns;
      Fmt.str "%.0fw" r3_get_alloc;
      Fmt.str "%.0fx" (r3_decode_ns /. r3_get_ns);
    ];

  Fmt.pr "\n";

  (* R1: 1 field *)
  let buf2 = Bytes.create 2 in
  let slice2 = Bs.make buf2 ~first:0 ~length:2 in

  let r1_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode codec1 buf2 0)
        done)
  in
  let r1_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get codec1 f_x slice2)
        done)
  in
  table_row widths
    [ "R1 decode (1 uint16)"; Fmt.str "%.1f" r1_decode_ns; Fmt.str "0w"; "-" ];
  table_row widths
    [
      "R1 zero-copy get";
      Fmt.str "%.1f" r1_get_ns;
      "0w";
      Fmt.str "%.0fx" (r1_decode_ns /. r1_get_ns);
    ];

  Fmt.pr "\n";

  (* Encode *)
  let clcw_val = clcw_default in
  let clcw_enc_buf = Bytes.create clcw_size in
  let clcw_encode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          Codec.encode clcw_codec clcw_val clcw_enc_buf 0
        done)
  in
  table_row widths [ "CLCW encode"; Fmt.str "%.1f" clcw_encode_ns; "0w"; "-" ];

  let clcw_set_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          Codec.set clcw_codec cw_report clcw_slice 42
        done)
  in
  table_row widths
    [
      "CLCW zero-copy set";
      Fmt.str "%.1f" clcw_set_ns;
      "0w";
      Fmt.str "%.0fx" (clcw_encode_ns /. clcw_set_ns);
    ];

  Fmt.pr "\n";

  (* ── Part 2: Allocation breakdown ── *)
  let widths =
    table_header "Allocation Breakdown"
      [ ("Operation", 30); ("words/call", 12) ]
  in

  let alloc_row name f =
    let w = alloc_words n f in
    table_row widths [ name; Fmt.str "%.1f" w ]
  in

  alloc_row "noop" (fun () -> ());
  alloc_row "Codec.decode codec1" (fun () ->
      ignore (Codec.decode codec1 buf2 0));
  alloc_row "Codec.decode codec3" (fun () ->
      ignore (Codec.decode codec3 buf6 0));
  alloc_row "Codec.decode clcw" (fun () ->
      ignore (Codec.decode clcw_codec clcw_buf 0));
  alloc_row "Codec.get (zero-copy)" (fun () ->
      ignore (Codec.get clcw_codec cw_report clcw_slice));
  alloc_row "Codec.set (zero-copy)" (fun () ->
      Codec.set clcw_codec cw_report clcw_slice 42);

  Fmt.pr "\n";

  let widths =
    table_header "Int boxing" [ ("Operation", 30); ("words/call", 12) ]
  in
  let alloc_row name f =
    let w = alloc_words n f in
    table_row widths [ name; Fmt.str "%.1f" w ]
  in
  let buf4 = Bytes.create 4 in
  alloc_row "Bytes.get_int32_be (boxed)" (fun () ->
      ignore (Bytes.get_int32_be buf4 0));
  alloc_row "Wire.UInt32.get_be (unboxed)" (fun () ->
      ignore (Wire.UInt32.get_be buf4 0));
  let buf8 = Bytes.create 8 in
  alloc_row "Bytes.get_int64_be (boxed)" (fun () ->
      ignore (Bytes.get_int64_be buf8 0));
  alloc_row "Wire.UInt63.get_be (unboxed)" (fun () ->
      ignore (Wire.UInt63.get_be buf8 0));

  Fmt.pr "\n";

  (* ── Part 3: Nested protocol ── *)
  let nested_buf = (nested_data 1).(0) in
  let nested_slice =
    Bs.make nested_buf ~first:0 ~length:(Bytes.length nested_buf)
  in

  let widths =
    table_header "Nested Protocol (outer 4B + inner 4B)"
      [ ("Operation", 38); ("ns/op", 8); ("alloc", 12) ]
  in

  (* Full decode: outer + inner (using decode for both layers) *)
  let nested_full_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode outer_hdr_codec nested_buf 0);
          ignore (Codec.decode inner_cmd_codec nested_buf 4)
        done)
  in
  let nested_full_alloc =
    alloc_words n (fun () ->
        ignore (Codec.decode outer_hdr_codec nested_buf 0);
        ignore (Codec.decode inner_cmd_codec nested_buf 4))
  in
  table_row widths
    [
      "Full decode (outer+inner)";
      Fmt.str "%.1f" nested_full_ns;
      Fmt.str "%.0fw" nested_full_alloc;
    ];

  (* Zero-copy: get outer length *)
  let nested_get_len_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get outer_hdr_codec f_oh_length nested_slice)
        done)
  in
  let nested_get_len_alloc =
    alloc_words n (fun () ->
        ignore (Codec.get outer_hdr_codec f_oh_length nested_slice))
  in
  table_row widths
    [
      "Zero-copy get outer.length";
      Fmt.str "%.1f" nested_get_len_ns;
      Fmt.str "%.0fw" nested_get_len_alloc;
    ];

  (* Zero-copy: get payload slice + inner field *)
  let nested_get_inner_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let payload = Codec.get outer_hdr_codec f_oh_payload nested_slice in
          ignore (Codec.get inner_cmd_codec f_cmd_id payload)
        done)
  in
  let nested_get_inner_alloc =
    alloc_words n (fun () ->
        let payload = Codec.get outer_hdr_codec f_oh_payload nested_slice in
        ignore (Codec.get inner_cmd_codec f_cmd_id payload))
  in
  table_row widths
    [
      "Zero-copy get outer.payload -> inner.id";
      Fmt.str "%.1f" nested_get_inner_ns;
      Fmt.str "%.0fw" nested_get_inner_alloc;
    ];

  (* Zero-copy: get inner seq through payload *)
  let nested_get_seq_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let payload = Codec.get outer_hdr_codec f_oh_payload nested_slice in
          ignore (Codec.get inner_cmd_codec f_cmd_seq payload)
        done)
  in
  let nested_get_seq_alloc =
    alloc_words n (fun () ->
        let payload = Codec.get outer_hdr_codec f_oh_payload nested_slice in
        ignore (Codec.get inner_cmd_codec f_cmd_seq payload))
  in
  table_row widths
    [
      "Zero-copy get outer.payload -> inner.seq";
      Fmt.str "%.1f" nested_get_seq_ns;
      Fmt.str "%.0fw" nested_get_seq_alloc;
    ];

  table_row widths
    [
      "  speedup vs full decode";
      Fmt.str "%.0fx" (nested_full_ns /. nested_get_inner_ns);
      Fmt.str "%.0fx"
        (if nested_get_inner_alloc > 0.0 then
           nested_full_alloc /. nested_get_inner_alloc
         else nested_full_alloc);
    ];

  Fmt.pr "\n";

  (* ── Part 4: TCP/IP nested zero-copy (Ethernet -> IPv4 -> TCP) ── *)
  let tcp_buf = (Net.tcp_frame_data 1).(0) in
  let tcp_frame = Bs.make tcp_buf ~first:0 ~length:(Bytes.length tcp_buf) in

  let widths =
    table_header "TCP/IP Nested Zero-Copy (Ethernet 14B -> IPv4 20B -> TCP 20B)"
      [ ("Operation", 42); ("ns/op", 8); ("alloc", 12) ]
  in

  (* Individual header decode *)
  let eth_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode Net.ethernet_codec tcp_buf 0)
        done)
  in
  let eth_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode Net.ethernet_codec tcp_buf 0))
  in
  table_row widths
    [
      "Ethernet decode (14B + 40B payload)";
      Fmt.str "%.1f" eth_decode_ns;
      Fmt.str "%.0fw" eth_decode_alloc;
    ];

  (* Zero-copy single field *)
  let eth_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get Net.ethernet_codec Net.f_eth_ethertype tcp_frame)
        done)
  in
  let eth_get_alloc =
    alloc_words n (fun () ->
        ignore (Codec.get Net.ethernet_codec Net.f_eth_ethertype tcp_frame))
  in
  table_row widths
    [
      "Codec.get Eth.ethertype";
      Fmt.str "%.1f" eth_get_ns;
      Fmt.str "%.0fw" eth_get_alloc;
    ];

  let ip_slice = Codec.get Net.ethernet_codec Net.f_eth_payload tcp_frame in
  let tcp_slice = Codec.get Net.ipv4_codec Net.f_ip_payload ip_slice in

  let ip_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get Net.ipv4_codec Net.f_ip_src ip_slice)
        done)
  in
  table_row widths
    [ "Codec.get IPv4.src (from sub-slice)"; Fmt.str "%.1f" ip_get_ns; "0w" ];

  let tcp_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp_slice)
        done)
  in
  table_row widths
    [
      "Codec.get TCP.dst_port (from sub-slice)"; Fmt.str "%.1f" tcp_get_ns; "0w";
    ];

  let syn_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get Net.tcp_codec Net.f_tcp_syn tcp_slice)
        done)
  in
  table_row widths
    [ "Codec.get TCP.syn (bool bitfield)"; Fmt.str "%.1f" syn_get_ns; "0w" ];

  Fmt.pr "\n";

  (* 3-layer traversal: frame -> TCP.dst_port *)
  let three_layer_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let ip = Codec.get Net.ethernet_codec Net.f_eth_payload tcp_frame in
          let tcp = Codec.get Net.ipv4_codec Net.f_ip_payload ip in
          ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp)
        done)
  in
  let three_layer_alloc =
    alloc_words n (fun () ->
        let ip = Codec.get Net.ethernet_codec Net.f_eth_payload tcp_frame in
        let tcp = Codec.get Net.ipv4_codec Net.f_ip_payload ip in
        ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp))
  in
  table_row widths
    [
      "3-layer: frame -> TCP.dst_port";
      Fmt.str "%.1f" three_layer_ns;
      Fmt.str "%.0fw" three_layer_alloc;
    ];

  (* 3-layer traversal using sub+get_raw: zero allocation *)
  let three_layer_raw_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let ip_off =
            Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0
          in
          let tcp_off =
            Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip_off
          in
          ignore
            (Codec.get_raw Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp_off)
        done)
  in
  let three_layer_raw_alloc =
    alloc_words n (fun () ->
        let ip_off = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp_off =
          Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip_off
        in
        ignore (Codec.get_raw Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp_off))
  in
  table_row widths
    [
      "3-layer: sub+get_raw -> TCP.dst_port";
      Fmt.str "%.1f" three_layer_raw_ns;
      Fmt.str "%.0fw" three_layer_raw_alloc;
    ];

  (* In-place mutation through sub-slices *)
  let set_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          Codec.set Net.tcp_codec Net.f_tcp_dst_port tcp_slice 8080
        done)
  in
  table_row widths
    [ "Codec.set TCP.dst_port (in-place)"; Fmt.str "%.1f" set_ns; "0w" ];

  table_row widths
    [
      "  sub+get_raw vs Ethernet decode";
      Fmt.str "%.0fx" (eth_decode_ns /. three_layer_raw_ns);
      Fmt.str "%.0fw->0w" eth_decode_alloc;
    ];

  Fmt.pr "\n";

  (* ── Part 5: Streaming parse/encode (uint32be = 4B, same as CLCW) ── *)
  let widths =
    table_header "Streaming Parse/Encode (uint32be 4B)"
      [ ("Operation", 42); ("ns/op", 8); ("alloc", 12) ]
  in

  let val32 = 0xDEADBEEF in
  let str32 = encode_to_string uint32be val32 in

  (* parse_string: single slice, no boundaries *)
  let parse_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (parse_string uint32be str32)
        done)
  in
  let parse_alloc =
    alloc_words n (fun () -> ignore (parse_string uint32be str32))
  in
  table_row widths
    [
      "parse_string (single slice)";
      Fmt.str "%.1f" parse_ns;
      Fmt.str "%.0fw" parse_alloc;
    ];

  (* parse chunked: 1 byte per slice, all boundaries straddled *)
  let parse_chunk1_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:1 str32 in
          ignore (parse uint32be reader)
        done)
  in
  let parse_chunk1_alloc =
    alloc_words n (fun () ->
        let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:1 str32 in
        ignore (parse uint32be reader))
  in
  table_row widths
    [
      "parse chunked (1B/slice, all straddle)";
      Fmt.str "%.1f" parse_chunk1_ns;
      Fmt.str "%.0fw" parse_chunk1_alloc;
    ];

  (* encode to string *)
  let encode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (encode_to_string uint32be val32)
        done)
  in
  let encode_alloc =
    alloc_words n (fun () -> ignore (encode_to_string uint32be val32))
  in
  table_row widths
    [
      "encode_to_string"; Fmt.str "%.1f" encode_ns; Fmt.str "%.0fw" encode_alloc;
    ];

  Fmt.pr "\n"
