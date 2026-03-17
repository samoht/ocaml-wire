(** TM frame reassembly benchmark.

    Simulates a downlink TM frame processor:
    - Pre-allocate a stream of TM Transfer Frames (1115-byte CADUs)
    - For each frame: parse the 6-byte header (11 bitfields), check VCID, read
      First Header Pointer, extract Space Packets from data field
    - Measure frames/sec and packets reassembled/sec

    Compares: pure C (shift/mask) vs Wire OCaml (staged Codec.get). *)

module C = Wire.Codec

let cadu_size = 1115
let tm_hdr = Space.tm_frame_size
let data_field_size = cadu_size - tm_hdr
let sp_hdr = Space.packet_size

let generate_frames n =
  let buf = Bytes.create (n * cadu_size) in
  let total_pkts = ref 0 in
  let data_off = ref tm_hdr in
  let remaining_in_frame = ref data_field_size in
  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    let vcid = frame mod 8 in
    let w0 = (0x1FF lsl 4) lor (vcid lsl 1) in
    Bytes.set_uint16_be buf base w0;
    Bytes.set_uint16_be buf (base + 2) ((frame mod 256) lsl 8);
    let first_hdr_ptr =
      if !remaining_in_frame >= pkt_size then 0 else !remaining_in_frame
    in
    let w2 = (1 lsl 14) lor (3 lsl 11) lor first_hdr_ptr in
    Bytes.set_uint16_be buf (base + 4) w2;
    data_off := base + tm_hdr;
    remaining_in_frame := data_field_size;
    while !remaining_in_frame >= pkt_size do
      let o = !data_off in
      let apid = !total_pkts mod 2048 in
      let w0 = (1 lsl 11) lor apid in
      Bytes.set_uint16_be buf o w0;
      Bytes.set_uint16_be buf (o + 2) (0xC000 lor (!total_pkts mod 16384));
      Bytes.set_uint16_be buf (o + 4) (pkt_payload - 1);
      data_off := o + pkt_size;
      remaining_in_frame := !remaining_in_frame - pkt_size;
      incr total_pkts
    done
  done;
  (buf, !total_pkts)

let () =
  let n = 1_000_000 in
  let buf, total_pkts = generate_frames n in
  Fmt.pr
    "TM frame reassembly (%d frames, %d-byte CADUs, %d embedded packets)\n\n" n
    cadu_size total_pkts;

  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in

  (* C baseline *)
  let c_ns = C_scenarios.gateway buf n cadu_size in
  let c_pkts = C_scenarios.gateway_pkts () in
  let c_dt = float c_ns /. 1e9 in
  let c_ns_per = float c_ns /. float n in

  (* Wire: staged get *)
  let get_vcid =
    Wire.Staged.unstage (C.get Space.tm_frame_codec Space.f_tf_vcid)
  in
  let get_fhp =
    Wire.Staged.unstage (C.get Space.tm_frame_codec Space.f_tf_first_hdr)
  in
  let get_apid =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_apid)
  in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_seq_count)
  in
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let w_pkts = ref 0 in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    let vcid = get_vcid buf base in
    let fhp = get_fhp buf base in
    ignore (Sys.opaque_identity vcid);
    let data_start = base + tm_hdr in
    let off = ref (data_start + fhp) in
    while !off + pkt_size <= data_start + data_field_size do
      let _apid = get_apid buf !off in
      let _seq = get_seq buf !off in
      off := !off + pkt_size;
      incr w_pkts
    done
  done;
  let w_dt = Unix.gettimeofday () -. t0 in
  let w_ns_per = w_dt *. 1e9 /. float n in
  let ratio = w_ns_per /. c_ns_per in

  Fmt.pr "  %-24s %5.0f ns/frm  %4.1f Mfrm/s  %5.1f Mpkt/s\n" "C (baseline)"
    c_ns_per
    (float n /. c_dt /. 1e6)
    (float c_pkts /. c_dt /. 1e6);
  Fmt.pr "  %-24s %5.0f ns/frm  %4.1f Mfrm/s  %5.1f Mpkt/s  (%.1fx)\n"
    "Wire (staged Codec.get)" w_ns_per
    (float n /. w_dt /. 1e6)
    (float !w_pkts /. w_dt /. 1e6)
    ratio;
  if c_pkts <> !w_pkts then
    Fmt.pr "\n  MISMATCH! C: %d pkts, Wire: %d pkts\n" c_pkts !w_pkts
  else Fmt.pr "\n  %d packets reassembled (C and Wire agree)\n" c_pkts
