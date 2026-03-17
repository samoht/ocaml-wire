(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words at frame rate:
    - Read Lockout, Wait, Retransmit flags (1-bit bitfields in bf_uint32be)
    - Read ReportValue for FARM-B counter (8-bit bitfield)
    - Compare with expected sequence number, flag anomalies

    Uses a contiguous buffer of packed 4-byte CLCW words (no Array.iter
    overhead, no per-element pointer chase).

    Compares: pure C (shift/mask) vs Wire OCaml (staged Codec.get). *)

module C = Wire.Codec

let n_words = 10_000_000
let n_rounds = 10
let word_size = Space.clcw_size (* 4 *)

(* Generate a contiguous buffer of n CLCW words using Codec.encode for
   correct bitfield layout. *)
let generate_stream n =
  let buf = Bytes.create (n * word_size) in
  let clcw_default = Space.clcw_default in
  for i = 0 to n - 1 do
    let off = i * word_size in
    Wire.Codec.encode Space.clcw_codec clcw_default buf off
  done;
  (* Patch individual fields using Codec.set for correct bit positions *)
  let set_lockout =
    Wire.Staged.unstage (C.set Space.clcw_codec Space.cw_lockout)
  in
  let set_wait = Wire.Staged.unstage (C.set Space.clcw_codec Space.cw_wait) in
  let set_retransmit =
    Wire.Staged.unstage (C.set Space.clcw_codec Space.cw_retransmit)
  in
  let set_report =
    Wire.Staged.unstage (C.set Space.clcw_codec Space.cw_report)
  in
  for i = 0 to n - 1 do
    let off = i * word_size in
    set_lockout buf off 0;
    set_wait buf off 0;
    set_retransmit buf off 0;
    set_report buf off (i mod 256)
  done;
  buf

let () =
  let buf = generate_stream n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each, contiguous buffer)\n\n" n_words
    word_size;

  let total_ops = n_words * n_rounds in

  (* C baseline *)
  let c_total_ns = ref 0 in
  for _ = 1 to n_rounds do
    c_total_ns := !c_total_ns + C_scenarios.clcw_contiguous buf n_words
  done;
  let c_anomalies = C_scenarios.clcw_anomalies () in
  let c_ns_per = float !c_total_ns /. float total_ops in

  (* Wire: staged Codec.get on contiguous buffer *)
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
  for _ = 1 to n_rounds do
    w_anomalies := 0;
    expected_seq := 0;
    for i = 0 to n_words - 1 do
      let off = i * word_size in
      let lockout = get_lockout buf off in
      let wait = get_wait buf off in
      let retransmit = get_retransmit buf off in
      let report = get_report buf off in
      if
        lockout <> 0 || wait <> 0 || retransmit <> 0
        || report <> !expected_seq land 0xFF
      then incr w_anomalies;
      expected_seq := report
    done
  done;
  let w_dt = Unix.gettimeofday () -. t0 in
  let w_ns_per = w_dt *. 1e9 /. float total_ops in
  let ratio = w_ns_per /. c_ns_per in

  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)\n"
    "C (baseline)" c_ns_per
    (float total_ops /. (float !c_total_ns /. 1e9) /. 1e6)
    c_anomalies;
  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)  (%.1fx)\n"
    "Wire (staged Codec.get)" w_ns_per
    (float total_ops /. w_dt /. 1e6)
    !w_anomalies ratio;
  if c_anomalies <> !w_anomalies then
    Fmt.pr "\n  MISMATCH! C: %d anomalies, Wire: %d anomalies\n" c_anomalies
      !w_anomalies
  else Fmt.pr "\n  %d anomalies detected (C and Wire agree)\n" c_anomalies
