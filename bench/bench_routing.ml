(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router:
    - Pre-allocate a contiguous buffer of N variable-size Space Packets
    - For each packet: read 6-byte primary header, extract APID (11-bit
      bitfield), look up routing table, extract variable-length payload via
      DataLength, pass payload pointer to handler (no copy)
    - Measure packets/sec and MB/s of payload delivered

    Compares: pure C (shift/mask) vs Wire OCaml (staged Codec.get). *)

module C = Wire.Codec

let apid_of_index i =
  let r = i mod 100 in
  if r < 40 then i mod 256
  else if r < 75 then 256 + (i mod 768)
  else if r < 95 then 1024 + (i mod 512)
  else 0x7FF

let payload_size_of_apid apid =
  if apid < 256 then 32
  else if apid < 1024 then 256
  else if apid < 1536 then 64
  else 8

let generate_stream n =
  let total = ref 0 in
  let hdr = Space.packet_size in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    total := !total + hdr + payload_size_of_apid apid
  done;
  let buf = Bytes.create !total in
  let off = ref 0 in
  let payload_total = ref 0 in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    let plen = payload_size_of_apid apid in
    let w0 = (1 lsl 11) lor apid in
    Bytes.set_uint16_be buf !off w0;
    let w1 = (3 lsl 14) lor (i mod 16384) in
    Bytes.set_uint16_be buf (!off + 2) w1;
    Bytes.set_uint16_be buf (!off + 4) (plen - 1);
    off := !off + hdr + plen;
    payload_total := !payload_total + plen
  done;
  (buf, !total, !payload_total)

let routing_table =
  Array.init 2048 (fun apid ->
      if apid < 256 then 0
      else if apid < 1024 then 1
      else if apid < 1536 then 2
      else 3)

let handler_counts = Array.make 4 0

let[@inline] dispatch handler_id =
  handler_counts.(handler_id) <- handler_counts.(handler_id) + 1

let () =
  let n = 10_000_000 in
  let buf, total_bytes, payload_bytes = generate_stream n in
  Fmt.pr "APID demux (%d packets, %d MB stream, %d MB payload)\n\n" n
    (total_bytes / 1_000_000)
    (payload_bytes / 1_000_000);

  let hdr = Space.packet_size in

  (* C baseline *)
  let c_ns = C_scenarios.routing buf n in
  let c_ns_per = float c_ns /. float n in
  let c_dt = float c_ns /. 1e9 in
  let c_mpps = float n /. c_dt /. 1e6 in
  let c_mbps = float payload_bytes /. c_dt /. 1e6 in

  (* Wire: staged get *)
  let get_apid =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_apid)
  in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_seq_count)
  in
  let get_dlen =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_data_len)
  in
  Gc.compact ();
  Array.fill handler_counts 0 4 0;
  let t0 = Unix.gettimeofday () in
  let off = ref 0 in
  for _ = 1 to n do
    let o = !off in
    let apid = get_apid buf o in
    let _seq = get_seq buf o in
    let dlen = get_dlen buf o in
    dispatch routing_table.(apid);
    off := o + hdr + dlen + 1
  done;
  let w_dt = Unix.gettimeofday () -. t0 in
  let w_ns_per = w_dt *. 1e9 /. float n in
  let w_mpps = float n /. w_dt /. 1e6 in
  let w_mbps = float payload_bytes /. w_dt /. 1e6 in
  let ratio = w_ns_per /. c_ns_per in

  Fmt.pr "  %-24s %5.1f ns/pkt  %5.1f Mpkt/s  %6.0f MB/s\n" "C (baseline)"
    c_ns_per c_mpps c_mbps;
  Fmt.pr "  %-24s %5.1f ns/pkt  %5.1f Mpkt/s  %6.0f MB/s  (%.1fx)\n"
    "Wire (staged Codec.get)" w_ns_per w_mpps w_mbps ratio;

  (* Validate C and Wire produce identical routing *)
  let c_hk = C_scenarios.routing_counts 0 in
  let c_sci = C_scenarios.routing_counts 1 in
  let c_diag = C_scenarios.routing_counts 2 in
  let c_idle = C_scenarios.routing_counts 3 in
  if
    c_hk <> handler_counts.(0)
    || c_sci <> handler_counts.(1)
    || c_diag <> handler_counts.(2)
    || c_idle <> handler_counts.(3)
  then
    Fmt.pr "\n  MISMATCH! C: hk=%d sci=%d diag=%d idle=%d\n" c_hk c_sci c_diag
      c_idle
  else
    Fmt.pr "\n  routed: hk=%d sci=%d diag=%d idle=%d (C and Wire agree)\n"
      handler_counts.(0) handler_counts.(1) handler_counts.(2)
      handler_counts.(3)
