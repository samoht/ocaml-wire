(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words at frame rate:
    - Read Lockout, Wait, Retransmit flags (1-bit bitfields in bf_uint32be)
    - Read ReportValue for FARM-B counter (8-bit bitfield)
    - Compare with expected sequence number, flag anomalies

    Compares: pure C (shift/mask) vs Wire OCaml (staged Codec.get). *)

module C = Wire.Codec

let n_words = 10_000_000

let () =
  let words = Space.clcw_data n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each)\n\n" n_words Space.clcw_size;

  (* C baseline *)
  let c_ns = C_scenarios.clcw words n_words in
  let c_anomalies = C_scenarios.clcw_anomalies () in
  let c_dt = float c_ns /. 1e9 in
  let c_ns_per = float c_ns /. float n_words in

  (* Wire: staged bitfield accessors *)
  let get_lockout =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_lockout)
  in
  let get_wait = Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_wait) in
  let get_retransmit =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_retransmit)
  in
  let get_report =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_report)
  in
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let w_anomalies = ref 0 in
  let expected_seq = ref 0 in
  Array.iter
    (fun buf ->
      let lockout = get_lockout buf 0 in
      let wait = get_wait buf 0 in
      let retransmit = get_retransmit buf 0 in
      let report = get_report buf 0 in
      if
        lockout <> 0 || wait <> 0 || retransmit <> 0
        || report <> !expected_seq land 0xFF
      then incr w_anomalies;
      expected_seq := report)
    words;
  let w_dt = Unix.gettimeofday () -. t0 in
  let w_ns_per = w_dt *. 1e9 /. float n_words in
  let ratio = w_ns_per /. c_ns_per in

  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)\n"
    "C (baseline)" c_ns_per
    (float n_words /. c_dt /. 1e6)
    c_anomalies;
  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)  (%.1fx)\n"
    "Wire (staged Codec.get)" w_ns_per
    (float n_words /. w_dt /. 1e6)
    !w_anomalies ratio;
  if c_anomalies <> !w_anomalies then
    Fmt.pr "\n  MISMATCH! C: %d anomalies, Wire: %d anomalies\n" c_anomalies
      !w_anomalies
  else Fmt.pr "\n  %d anomalies detected (C and Wire agree)\n" c_anomalies
