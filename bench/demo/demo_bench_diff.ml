(* Differential test: demo bench cases tested against EverParse C stubs
   via Wire_diff. The C parse function is registered into Read_case.c_parse,
   then diff_of_case builds a Wire_diff harness from the Read_case alone. *)

open Wire
open Demo_bench_cases

type case = {
  id : Demo_bench_cases.id;
  label : string;
  packed : Wire_diff.packed_test;
}

(* C raw record types returned by generated stubs *)
type minimal_raw = { value : int }
type all_ints_raw = { u64be : int64 }
type large_mixed_raw = { timestamp : int64 }
type bf8_raw = { value : int }
type bf16_raw = { bf16_id : int }
type bf32_raw = { priority : int }
type bool_raw = { active : int }
type clcw_raw = { report_value : int }
type packet_raw = { apid : int }
type ipv4_raw = { src_addr : int }
type tcp_raw = { dst_port : int }
type tcp_syn_raw = { syn : int }
type mapped_raw = { priority : int }
type cases_raw = { packet_type : int }
type enum_raw = { status_code : int }
type constrained_raw = { data : int }

let ptype_of_int = function 0 -> Demo.Telemetry | _ -> Demo.Telecommand

let priority_of_int = function
  | 0 -> Demo.Low
  | 1 -> Demo.Medium
  | 2 -> Demo.High
  | _ -> Demo.Critical

let status_of_int = function 0 -> `Ok | 1 -> `Warn | 2 -> `Err | _ -> `Crit

(* Register C parse functions into Read_case.c_parse.
   This runs at module init time, before any diff tests. *)
let () =
  let reg id parse =
    let (Read_case c) =
      List.find (fun (Read_case c) -> c.id = id) projection_cases
    in
    c.c_parse <- Some parse
  in
  reg Minimal (fun buf -> (C_stubs.minimal_parse buf : minimal_raw).value);
  reg All_ints (fun buf -> (C_stubs.allints_parse buf : all_ints_raw).u64be);
  reg Large_mixed (fun buf ->
      (C_stubs.largemixed_parse buf : large_mixed_raw).timestamp);
  reg Bitfield8 (fun buf -> (C_stubs.bitfield8_parse buf : bf8_raw).value);
  reg Bitfield16 (fun buf -> (C_stubs.bitfield16_parse buf : bf16_raw).bf16_id);
  reg Bitfield32 (fun buf -> (C_stubs.bitfield32_parse buf : bf32_raw).priority);
  reg Bool_fields (fun buf ->
      (C_stubs.boolfields_parse buf : bool_raw).active <> 0);
  reg Clcw_report (fun buf ->
      (C_stubs.clcwreport_parse buf : clcw_raw).report_value);
  reg Space_packet_apid (fun buf ->
      (C_stubs.spacepacketapid_parse buf : packet_raw).apid);
  reg Ipv4_src (fun buf -> (C_stubs.ipv4_parse buf : ipv4_raw).src_addr);
  reg Tcp_dst_port (fun buf -> (C_stubs.tcp_parse buf : tcp_raw).dst_port);
  reg Tcp_syn (fun buf -> (C_stubs.tcpsyn_parse buf : tcp_syn_raw).syn <> 0);
  reg Mapped_priority (fun buf ->
      priority_of_int (C_stubs.mapped_parse buf : mapped_raw).priority);
  reg Cases_type (fun buf ->
      ptype_of_int (C_stubs.casesdemo_parse buf : cases_raw).packet_type);
  reg Enum_status (fun buf ->
      status_of_int (C_stubs.enumdemo_parse buf : enum_raw).status_code);
  reg Constrained_data (fun buf ->
      (C_stubs.constrained_parse buf : constrained_raw).data)

(* Build a diff case from a Read_case that has c_parse registered. *)
let diff_of_case (Read_case c) =
  let parse =
    match c.c_parse with
    | Some p -> p
    | None -> failwith ("no C parse registered for " ^ c.label)
  in
  let stubs = C_stubs.stubs_of_name (Everparse.Raw.struct_name c.struct_) in
  let size = Codec.wire_size c.codec in
  let read buf =
    let bytes = Bytes.of_string buf in
    if not (stubs.check bytes) then None else Some (parse bytes)
  in
  let ocaml_read buf =
    if String.length buf < size then None
    else Some (c.get (Bytes.of_string buf) 0)
  in
  let project v =
    let buf = Bytes.create size in
    Codec.encode c.codec v buf 0;
    c.get buf 0
  in
  let write _ = None in
  let packed =
    Wire_diff.pack
      (Wire_diff.harness ~name:c.label ~codec:c.codec ~read ~write ~project
         ~equal:c.equal ~ocaml_read ())
  in
  { id = c.id; label = c.label; packed }

let cases = List.map diff_of_case projection_cases
let find_case id = List.find (fun c -> c.id = id) cases

let verify_of_id id =
  let c = find_case id in
  let (Read_case r) =
    List.find (fun (Read_case r) -> r.id = id) projection_cases
  in
  fun () ->
    Array.iteri
      (fun i item ->
        let buf = Bytes.unsafe_to_string item in
        match c.packed.test_read buf with
        | Wire_diff.Match -> ()
        | Wire_diff.Both_failed ->
            Fmt.failwith "%s: read failed on item %d" c.label i
        | Wire_diff.Value_mismatch msg ->
            Fmt.failwith "%s: read mismatch on item %d: %s" c.label i msg
        | Wire_diff.Only_c_ok msg ->
            Fmt.failwith "%s: only C succeeded on item %d: %s" c.label i msg
        | Wire_diff.Only_ocaml_ok msg ->
            Fmt.failwith "%s: only OCaml succeeded on item %d: %s" c.label i msg)
      r.dataset.items
