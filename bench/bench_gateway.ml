(** TM frame reassembly benchmark.

    Simulates a downlink TM frame processor:
    - Pre-allocate a stream of TM Transfer Frames (1115-byte CADUs)
    - For each frame: parse the 6-byte header (11 bitfields), check VCID to
      select virtual channel, read First Header Pointer to find the first Space
      Packet, extract packets from the data field (packets may span frames)
    - Measure frames/sec and packets reassembled/sec

    Compares Wire zero-copy get vs hand-written OCaml Bytes (shift/mask). *)

module C = Wire.Codec

(* CADU = 1115 bytes. TM frame header = 6 bytes. Data field = 1109 bytes. *)
let cadu_size = 1115
let tm_hdr = Space.tm_frame_size (* 6 *)
let data_field_size = cadu_size - tm_hdr (* 1109 *)
let sp_hdr = Space.packet_size (* 6 *)

(* Generate a stream of n CADUs with embedded Space Packets. Packets are packed
   contiguously across frame boundaries. Returns (buf, total_embedded_packets). *)
let generate_frames n =
  let buf = Bytes.create (n * cadu_size) in
  let total_pkts = ref 0 in
  (* Walk through data fields, packing Space Packets *)
  let data_off = ref tm_hdr in
  let remaining_in_frame = ref data_field_size in
  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    (* Write TM frame header *)
    (* w0: version=0, SCID=0x1FF (10 bits), VCID=frame%8 (3 bits), OCF=0 *)
    let vcid = frame mod 8 in
    let w0 = (0x1FF lsl 4) lor (vcid lsl 1) in
    Bytes.set_uint16_be buf base w0;
    (* w1: MCCount, VCCount *)
    Bytes.set_uint16_be buf (base + 2) ((frame mod 256) lsl 8);
    (* w2: SecHdr=0, Sync=1, PktOrder=0, SegId=3, FirstHdrPtr *)
    let first_hdr_ptr =
      if !remaining_in_frame >= pkt_size then 0 else !remaining_in_frame
    in
    let w2 = (1 lsl 14) lor (3 lsl 11) lor first_hdr_ptr in
    Bytes.set_uint16_be buf (base + 4) w2;
    (* Pack packets into this frame's data field *)
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

  let time label f =
    Gc.compact ();
    let t0 = Unix.gettimeofday () in
    let pkts = f () in
    let dt = Unix.gettimeofday () -. t0 in
    let ns_per_frame = dt *. 1e9 /. float n in
    let mfps = float n /. dt /. 1e6 in
    let mpps = float pkts /. dt /. 1e6 in
    Fmt.pr "  %-45s %5.0f ns/frm  %4.1f Mfrm/s  %4.1f Mpkt/s\n" label
      ns_per_frame mfps mpps
  in

  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in

  (* Wire: zero-copy field access *)
  time "wire: get VCID + FirstHdrPtr + walk pkts" (fun () ->
      let pkts = ref 0 in
      for frame = 0 to n - 1 do
        let base = frame * cadu_size in
        let vcid = C.get Space.tm_frame_codec Space.f_tf_vcid buf base in
        let fhp = C.get Space.tm_frame_codec Space.f_tf_first_hdr buf base in
        ignore (Sys.opaque_identity vcid);
        (* Walk packets starting at First Header Pointer *)
        let data_start = base + tm_hdr in
        let off = ref (data_start + fhp) in
        while !off + pkt_size <= data_start + data_field_size do
          let _apid = C.get Space.packet_codec Space.f_sp_apid buf !off in
          let _seq = C.get Space.packet_codec Space.f_sp_seq_count buf !off in
          off := !off + pkt_size;
          incr pkts
        done
      done;
      !pkts);

  (* Hand-written: hardcoded offsets + shift/mask *)
  time "hand: Bytes.get_uint16_be + shift/mask" (fun () ->
      let pkts = ref 0 in
      for frame = 0 to n - 1 do
        let base = frame * cadu_size in
        (* VCID: bits [4:1] of first uint16be (3 bits after 10-bit SCID) *)
        let w0 = Bytes.get_uint16_be buf base in
        let vcid = (w0 lsr 1) land 0x7 in
        (* FirstHdrPtr: low 11 bits of third uint16be *)
        let w2 = Bytes.get_uint16_be buf (base + 4) in
        let fhp = w2 land 0x7FF in
        ignore (Sys.opaque_identity vcid);
        let data_start = base + tm_hdr in
        let off = ref (data_start + fhp) in
        while !off + pkt_size <= data_start + data_field_size do
          let w0 = Bytes.get_uint16_be buf !off in
          let _apid = w0 land 0x7FF in
          let _seq = Bytes.get_uint16_be buf (!off + 2) land 0x3FFF in
          off := !off + pkt_size;
          incr pkts
        done
      done;
      !pkts)
