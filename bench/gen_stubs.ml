(** Generate EverParse .3d files, run EverParse, and generate C/OCaml FFI stubs
    and benchmark loops for the schemas used in bench_demo.ml.

    Usage: gen_stubs.exe <schema_dir>

    Outputs:
    - <schema_dir>/*.3d -- EverParse 3D schema files
    - c_stubs.c -- C validation stubs + noop stubs + timed C loops
    - c_stubs.ml -- OCaml externals for all stubs *)

let schema_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"

type entry = E : string * 'r Wire.Codec.t * Wire.C.Raw.struct_ -> entry

let entries =
  [
    (* Demo: synthetic schemas covering every Wire type *)
    E ("Minimal", Demo.minimal_codec, Demo.minimal_struct);
    E ("Bitfield8", Demo.bf8_codec, Demo.bf8_struct);
    E ("Bitfield16", Demo.bf16_codec, Demo.bf16_struct);
    E ("BoolFields", Demo.bool_fields_codec, Demo.bool_fields_struct);
    E ("Bitfield32", Demo.bf32_codec, Demo.bf32_struct);
    E ("AllInts", Demo.all_ints_codec, Demo.all_ints_struct);
    E ("LargeMixed", Demo.large_mixed_codec, Demo.large_mixed_struct);
    (* Demo: type combinators *)
    E ("Mapped", Demo.mapped_codec, Demo.mapped_struct);
    E ("CasesDemo", Demo.cases_demo_codec, Demo.cases_demo_struct);
    E ("EnumDemo", Demo.enum_demo_codec, Demo.enum_demo_struct);
    E ("Constrained", Demo.constrained_codec, Demo.constrained_struct);
    (* Space: real protocols *)
    E ("CLCW", Space.clcw_codec, Space.clcw_struct);
    E ("SpacePacket", Space.packet_codec, Space.packet_struct);
    E ("TMFrame", Space.tm_frame_codec, Space.tm_frame_struct);
    (* Net: TCP/IP headers *)
    E ("Ethernet", Net.ethernet_codec, Net.ethernet_struct);
    E ("IPv4", Net.ipv4_codec, Net.ipv4_struct);
    E ("TCP", Net.tcp_codec, Net.tcp_struct);
  ]

let () =
  let structs = List.map (fun (E (_, _, s)) -> s) entries in
  let schemas =
    List.map (fun (E (_, codec, _)) -> Wire.C.schema ~output:true codec) entries
  in

  (* Step 1: Generate .3d files and run EverParse *)
  Wire_3d.generate_3d ~outdir:schema_dir schemas;
  let quiet = Sys.getenv_opt "EVERPARSE_VERBOSE" = None in
  Wire_3d.run_everparse ~quiet ~outdir:schema_dir schemas;

  (* Step 2: Generate c_stubs.c *)
  let oc = open_out "c_stubs.c" in
  output_string oc (Wire_stubs.to_c_stubs structs);
  let pr fmt = Printf.fprintf oc fmt in

  pr "\n/* ── Noop FFI stubs (measure call overhead) ── */\n\n";
  pr "CAMLprim value ep_noop(value v_buf) {\n";
  pr "  (void)v_buf;\n";
  pr "  return Val_true;\n";
  pr "}\n\n";
  pr "CAMLprim value ep_noop_safe(value v_buf) {\n";
  pr "  CAMLparam1(v_buf);\n";
  pr "  CAMLreturn(Val_true);\n";
  pr "}\n\n";

  pr "\n/* ── Timed C benchmark loops ── */\n\n";
  pr "#include <time.h>\n\n";
  pr "static inline int64_t now_ns(void) {\n";
  pr "  struct timespec ts;\n";
  pr "  clock_gettime(CLOCK_MONOTONIC, &ts);\n";
  pr "  return ts.tv_sec * 1000000000LL + ts.tv_nsec;\n";
  pr "}\n\n";
  pr "static void bench_err(const char *t, const char *f, const char *r,\n";
  pr "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {\n";
  pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
  pr "}\n\n";
  List.iter
    (fun s ->
      let name = Wire.C.Raw.struct_name s in
      let ep = Wire_3d.everparse_name name in
      let lower = String.lowercase_ascii name in
      pr "CAMLprim value ep_loop_%s(value v_buf, value v_off, value v_n) {\n"
        lower;
      pr "  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);\n";
      pr "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);\n";
      pr "  int count = Int_val(v_n);\n";
      pr "  volatile uint64_t result;\n";
      pr "  int64_t t0 = now_ns();\n";
      pr "  for (int i = 0; i < count; i++) {\n";
      pr "    result = %sValidate%s(NULL, bench_err, buf, len, 0);\n" ep ep;
      pr "  }\n";
      pr "  (void)result;\n";
      pr "  int64_t t1 = now_ns();\n";
      pr "  return Val_int(t1 - t0);\n";
      pr "}\n\n")
    structs;

  close_out oc;

  (* Step 3: Generate c_stubs.ml *)
  let oc = open_out "c_stubs.ml" in
  output_string oc (Wire_stubs.to_ml_stubs structs);
  let pr fmt = Printf.fprintf oc fmt in
  pr "(* Noop FFI stubs *)\n\n";
  pr "external noop : bytes -> bool = \"ep_noop\" [@@noalloc]\n\n";
  pr "external noop_safe : bytes -> bool = \"ep_noop_safe\"\n\n";
  pr "(* Timed C benchmark loops *)\n\n";
  List.iter
    (fun s ->
      let lower = String.lowercase_ascii (Wire.C.Raw.struct_name s) in
      pr "external %s_loop : bytes -> int -> int -> int = \"ep_loop_%s\"\n\n"
        lower lower)
    structs;
  close_out oc;

  Printf.printf "Generated %d schemas in %s/\n" (List.length structs) schema_dir;
  Printf.printf "Generated c_stubs.c, c_stubs.ml\n"
