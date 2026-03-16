(** Comparison: Wire vs mirage-tcpip for deep packet inspection.

    Task: extract a field from the TCP layer of a raw Ethernet frame. This
    requires traversing Ethernet -> IPv4 -> TCP using each library's actual API
    — no hand-rolled offset arithmetic.

    Compared approaches: 1. Wire Codec.get -- zero-copy sub-slice traversal (the
    Wire API) 2. mirage-tcpip -- Unmarshal.of_cstruct at each layer (the mirage
    API)

    Usage: dune exec bench/compare/compare.exe [-- ITERATIONS] *)

module Bs = Bytesrw.Bytes.Slice

(* ── Timing ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float_of_int n

let alloc_words n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

(* ── Test data ── *)

let frame_buf = (Net.tcp_frame_data 1).(0)
let frame_cstruct = Cstruct.of_bytes frame_buf
let frame_slice = Bs.make frame_buf ~first:0 ~length:(Bytes.length frame_buf)

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

  (* ── Part 1: Extract TCP dst_port from raw Ethernet frame ── *)
  let widths =
    table_header "Extract TCP dst_port from raw Ethernet frame (54B)"
      [ ("Library", 40); ("ns/op", 8); ("alloc", 10); ("vs Wire", 8) ]
  in

  (* Wire zero-copy: 3 sub-slice traversals *)
  let wire_zc_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let ip =
            Wire.Codec.get Net.ethernet_codec Net.f_eth_payload frame_slice
          in
          let tcp = Wire.Codec.get Net.ipv4_codec Net.f_ip_payload ip in
          ignore (Wire.Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp)
        done)
  in
  let wire_zc_alloc =
    alloc_words n (fun () ->
        let ip =
          Wire.Codec.get Net.ethernet_codec Net.f_eth_payload frame_slice
        in
        let tcp = Wire.Codec.get Net.ipv4_codec Net.f_ip_payload ip in
        ignore (Wire.Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp))
  in
  table_row widths
    [
      "Wire Codec.get (3 layers)";
      Fmt.str "%.1f" wire_zc_ns;
      Fmt.str "%.0fw" wire_zc_alloc;
      "-";
    ];

  (* mirage-tcpip: Ethernet -> IPv4 -> Tcp full unmarshal *)
  let mirage_full_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          match Ethernet.Packet.of_cstruct frame_cstruct with
          | Error _ -> ()
          | Ok (_eth, ip_cs) -> (
              match Ipv4_packet.Unmarshal.of_cstruct ip_cs with
              | Error _ -> ()
              | Ok (_ipv4, tcp_cs) -> (
                  match Tcp.Tcp_packet.Unmarshal.of_cstruct tcp_cs with
                  | Error _ -> ()
                  | Ok (tcp, _payload) -> ignore tcp.Tcp.Tcp_packet.dst_port))
        done)
  in
  let mirage_full_alloc =
    alloc_words n (fun () ->
        match Ethernet.Packet.of_cstruct frame_cstruct with
        | Error _ -> ()
        | Ok (_eth, ip_cs) -> (
            match Ipv4_packet.Unmarshal.of_cstruct ip_cs with
            | Error _ -> ()
            | Ok (_ipv4, tcp_cs) -> (
                match Tcp.Tcp_packet.Unmarshal.of_cstruct tcp_cs with
                | Error _ -> ()
                | Ok (tcp, _payload) -> ignore tcp.Tcp.Tcp_packet.dst_port)))
  in
  table_row widths
    [
      "mirage Unmarshal (3 layers)";
      Fmt.str "%.1f" mirage_full_ns;
      Fmt.str "%.0fw" mirage_full_alloc;
      Fmt.str "%.1fx" (mirage_full_ns /. wire_zc_ns);
    ];

  Fmt.pr "\n";

  (* ── Part 2: Extract TCP SYN flag ── *)
  let widths =
    table_header "Extract TCP SYN flag from raw Ethernet frame"
      [ ("Library", 40); ("ns/op", 8); ("alloc", 10); ("vs Wire", 8) ]
  in

  let wire_syn_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let ip =
            Wire.Codec.get Net.ethernet_codec Net.f_eth_payload frame_slice
          in
          let tcp = Wire.Codec.get Net.ipv4_codec Net.f_ip_payload ip in
          ignore (Wire.Codec.get Net.tcp_codec Net.f_tcp_syn tcp)
        done)
  in
  let wire_syn_alloc =
    alloc_words n (fun () ->
        let ip =
          Wire.Codec.get Net.ethernet_codec Net.f_eth_payload frame_slice
        in
        let tcp = Wire.Codec.get Net.ipv4_codec Net.f_ip_payload ip in
        ignore (Wire.Codec.get Net.tcp_codec Net.f_tcp_syn tcp))
  in
  table_row widths
    [
      "Wire Codec.get (3 layers)";
      Fmt.str "%.1f" wire_syn_ns;
      Fmt.str "%.0fw" wire_syn_alloc;
      "-";
    ];

  let mirage_syn_full_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          match Ethernet.Packet.of_cstruct frame_cstruct with
          | Error _ -> ()
          | Ok (_eth, ip_cs) -> (
              match Ipv4_packet.Unmarshal.of_cstruct ip_cs with
              | Error _ -> ()
              | Ok (_ipv4, tcp_cs) -> (
                  match Tcp.Tcp_packet.Unmarshal.of_cstruct tcp_cs with
                  | Error _ -> ()
                  | Ok (tcp, _payload) -> ignore tcp.Tcp.Tcp_packet.syn))
        done)
  in
  let mirage_syn_full_alloc =
    alloc_words n (fun () ->
        match Ethernet.Packet.of_cstruct frame_cstruct with
        | Error _ -> ()
        | Ok (_eth, ip_cs) -> (
            match Ipv4_packet.Unmarshal.of_cstruct ip_cs with
            | Error _ -> ()
            | Ok (_ipv4, tcp_cs) -> (
                match Tcp.Tcp_packet.Unmarshal.of_cstruct tcp_cs with
                | Error _ -> ()
                | Ok (tcp, _payload) -> ignore tcp.Tcp.Tcp_packet.syn)))
  in
  table_row widths
    [
      "mirage Unmarshal (3 layers)";
      Fmt.str "%.1f" mirage_syn_full_ns;
      Fmt.str "%.0fw" mirage_syn_full_alloc;
      Fmt.str "%.1fx" (mirage_syn_full_ns /. wire_syn_ns);
    ];

  Fmt.pr "\n";

  (* ── Notes ── *)
  Fmt.pr "Note: Wire operates on Bytes, mirage-tcpip on Cstruct (Bigarray).\n";
  Fmt.pr "      Data is pre-allocated in each format; conversion excluded.\n";
  Fmt.pr "\n";
  Fmt.pr "      Wire Codec.get traverses layers via zero-copy sub-slicing:\n";
  Fmt.pr
    "      each layer returns a Bytesrw.Bytes.Slice.t sub-view (4 words),\n";
  Fmt.pr "      final field read is a direct unsafe byte access.\n";
  Fmt.pr "\n";
  Fmt.pr "      mirage Unmarshal fully decodes each layer into OCaml records\n";
  Fmt.pr "      (Ethernet_packet.t, Ipv4_packet.t, Tcp_packet.t) with MAC\n";
  Fmt.pr "      address parsing, header validation, and payload extraction.\n"
