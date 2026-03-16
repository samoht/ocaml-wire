(** Performance comparison: EverParse C vs OCaml Wire.Codec vs OCaml->C FFI.

    All protocols defined once in OCaml Wire DSL. Three columns: 1. EverParse C
    -- generated verified C validator, timed in pure C loop 2. OCaml Codec --
    pure OCaml Wire.Codec.decode 3. OCaml->C FFI -- OCaml calling EverParse C
    validator via generated stubs

    Plus zero-copy get benchmarks for nested protocol access.

    Usage: BUILD_EVERPARSE=1 dune exec bench/tcpip/bench.exe [-- ITERATIONS] *)

open Net
module Bs = Bytesrw.Bytes.Slice

(* ── Timing ── *)

let time_ns f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9

(* ── Benchmarks ── *)

let ocaml_decode (type a) (s : a schema) n =
  let buf = (tcp_frame_data 1).(0) in
  let ns =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (s.decode buf 0)
        done)
  in
  ns /. float_of_int n

let ffi_check (check : bytes -> bool) n =
  let buf = (tcp_frame_data 1).(0) in
  let ns =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (check buf)
        done)
  in
  ns /. float_of_int n

let c_loop (loop : bytes -> int -> int -> int) (s : _ schema) n =
  let buf = (tcp_frame_data 1).(0) in
  let ns = loop buf 0 n in
  float_of_int ns /. float_of_int n

(* ── Output formatting ── *)

let header () =
  Fmt.pr "TCP/IP Wire Codec Performance: EverParse C vs OCaml vs C FFI\n";
  Fmt.pr "=============================================================\n";
  Fmt.pr "All protocols defined once in OCaml Wire DSL.\n\n";
  Fmt.pr "%-14s %4s  %-14s %10s %10s\n" "Protocol" "Size" "Method" "Validate"
    "vs C";
  Fmt.pr "%-14s %4s  %-14s %10s %10s\n" "" "" "" "ns/op" "";
  Fmt.pr "%s\n" (String.make 58 '-')

let row ?(schema = "") ?(size = 0) method_ time c_baseline =
  let ratio =
    if c_baseline > 0.1 then Fmt.str "%.1fx" (time /. c_baseline) else "-"
  in
  if schema <> "" then
    Fmt.pr "%-14s %3dB  %-14s %8.1f %10s\n" schema size method_ time ratio
  else Fmt.pr "%-14s %4s  %-14s %8.1f %10s\n" "" "" method_ time ratio

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in

  header ();

  (* Per-protocol: EverParse C vs OCaml Codec vs FFI *)
  List.iter
    (fun (Any s as _any) ->
      let lower = String.lowercase_ascii s.name in
      let c_loop_fn = Tcpip_ep_dispatch.get_loop lower in
      let ffi_check_fn = Tcpip_ep_dispatch.get_check lower in
      let c_time = c_loop c_loop_fn s (n * 10) in
      let ocaml_time = ocaml_decode s n in
      let ffi_time = ffi_check ffi_check_fn n in
      row ~schema:s.name ~size:s.size "EverParse C" c_time c_time;
      row "OCaml Codec" ocaml_time c_time;
      row "OCaml->C FFI" ffi_time c_time;
      Fmt.pr "\n")
    all_schemas;

  (* Zero-copy nested access *)
  Fmt.pr "\nNested Zero-Copy Access (Ethernet -> IPv4 -> TCP)\n";
  Fmt.pr "=================================================\n\n";
  Fmt.pr "%-40s %10s\n" "Operation" "ns/op";
  Fmt.pr "%s\n" (String.make 52 '-');

  let buf = (tcp_frame_data 1).(0) in
  let frame = Bs.make buf ~first:0 ~length:(Bytes.length buf) in
  let ip_slice = Wire.Codec.get ethernet_codec f_eth_payload frame in
  let tcp_slice = Wire.Codec.get ipv4_codec f_ip_payload ip_slice in

  (* Single-field zero-copy get *)
  let zc_etype =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get ethernet_codec f_eth_ethertype frame)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.get Eth.ethertype" zc_etype;

  let zc_protocol =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get ipv4_codec f_ip_protocol ip_slice)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.get IPv4.protocol (from sub-slice)" zc_protocol;

  let zc_src =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get ipv4_codec f_ip_src ip_slice)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.get IPv4.src_addr (from sub-slice)" zc_src;

  let zc_dst_port =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get tcp_codec f_tcp_dst_port tcp_slice)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.get TCP.dst_port (from sub-slice)" zc_dst_port;

  let zc_syn =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get tcp_codec f_tcp_syn tcp_slice)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.get TCP.syn (bool bitfield)" zc_syn;

  (* 3-layer traversal *)
  let zc_3layer =
    time_ns (fun () ->
        for _ = 1 to n do
          let ip = Wire.Codec.get ethernet_codec f_eth_payload frame in
          let tcp = Wire.Codec.get ipv4_codec f_ip_payload ip in
          ignore (Wire.Codec.get tcp_codec f_tcp_dst_port tcp)
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "3-layer: frame -> TCP.dst_port" zc_3layer;

  (* In-place mutation *)
  let zc_set =
    time_ns (fun () ->
        for _ = 1 to n do
          Wire.Codec.set tcp_codec f_tcp_dst_port tcp_slice 8080
        done)
    /. float_of_int n
  in
  Fmt.pr "%-40s %8.1f\n" "Codec.set TCP.dst_port (in-place)" zc_set;

  Fmt.pr "\n"
