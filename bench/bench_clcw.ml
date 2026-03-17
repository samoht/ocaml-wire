(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words at frame rate:
    - Read Lockout, Wait, Retransmit flags (1-bit bitfields in bf_uint32be)
    - Read ReportValue for FARM-B counter (8-bit bitfield)
    - Compare with expected sequence number
    - Flag anomaly on mismatch or error flags set

    All 4 reads are from a single 32-bit word (bf_uint32be), exercising the
    bitfield closure dispatch path.

    Compares Wire zero-copy get vs hand-written OCaml Bytes (shift/mask). *)

module C = Wire.Codec

let n_words = 10_000_000

let time label f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let anomalies = f () in
  let dt = Unix.gettimeofday () -. t0 in
  let ns_per = dt *. 1e9 /. float n_words in
  let mcps = float n_words /. dt /. 1e6 in
  Fmt.pr "  %-50s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)\n" label ns_per
    mcps anomalies

let () =
  let words = Space.clcw_data n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each)\n\n" n_words Space.clcw_size;

  (* Wire: bitfield accessors (partial-apply get outside loop) *)
  let get_lockout = C.get Space.clcw_codec Space.cw_lockout in
  let get_wait = C.get Space.clcw_codec Space.cw_wait in
  let get_retransmit = C.get Space.clcw_codec Space.cw_retransmit in
  let get_report = C.get Space.clcw_codec Space.cw_report in
  time "wire: get lockout+wait+retransmit+report" (fun () ->
      let anomalies = ref 0 in
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
          then incr anomalies;
          expected_seq := report)
        words;
      !anomalies);

  (* Hand-written: single 32-bit read + mask/shift *)
  time "hand: get_int32_be + mask/shift x4" (fun () ->
      let anomalies = ref 0 in
      let expected_seq = ref 0 in
      Array.iter
        (fun buf ->
          let w = Bytes.get_int32_be buf 0 |> Int32.to_int in
          (* CLCW layout: ...| Lockout(1) | Wait(1) | Retransmit(1) |
             FARM-B(2) | ReportValue(8) | *)
          let lockout = (w lsr 8) land 1 in
          let wait = (w lsr 7) land 1 in
          let retransmit = (w lsr 6) land 1 in
          let report = w land 0xFF in
          if
            lockout <> 0 || wait <> 0 || retransmit <> 0
            || report <> !expected_seq land 0xFF
          then incr anomalies;
          expected_seq := report)
        words;
      !anomalies)
