(** Synthetic benchmark schemas exercising all Wire API constructs.

    These schemas are designed for benchmarking and testing coverage, not based
    on real protocols. They cover:

    - Integer types: uint8, uint16, uint16be, uint32, uint32be, uint64be
    - Bitfields: bf_uint8 (8 bits), bf_uint16be (16 bits), bf_uint32be (32 bits)
    - Type combinators: map, bool
    - Various struct sizes: 1B to 26B *)

open Wire

(* ── 1. Minimal: single uint8 = 1 byte ── *)

type minimal = { m_value : int }

let f_minimal_value = Codec.field "Value" uint8 (fun m -> m.m_value)

let minimal_codec =
  Codec.view "Minimal" (fun v -> { m_value = v }) Codec.[ f_minimal_value ]

let minimal_struct = Codec.to_struct minimal_codec
let minimal_size = Codec.wire_size minimal_codec
let minimal_default = { m_value = 42 }

let minimal_data n =
  Array.init n (fun i ->
      let b = Bytes.create minimal_size in
      Bytes.set_uint8 b 0 (i mod 256);
      b)

(* ── 2. AllInts: all integer widths = 1+2+2+4+4+8 = 21 bytes ── *)

type all_ints = {
  ai_u8 : int;
  ai_u16 : int;
  ai_u16be : int;
  ai_u32 : int;
  ai_u32be : int;
  ai_u64be : int64;
}

let f_ints_u64be = Codec.field "U64BE" uint64be (fun a -> a.ai_u64be)

let all_ints_codec =
  Codec.view "AllInts"
    (fun u8 u16 u16be u32 u32be u64be ->
      {
        ai_u8 = u8;
        ai_u16 = u16;
        ai_u16be = u16be;
        ai_u32 = u32;
        ai_u32be = u32be;
        ai_u64be = u64be;
      })
    Codec.
      [
        Codec.field "U8" uint8 (fun a -> a.ai_u8);
        Codec.field "U16" uint16 (fun a -> a.ai_u16);
        Codec.field "U16BE" uint16be (fun a -> a.ai_u16be);
        Codec.field "U32" uint32 (fun a -> a.ai_u32);
        Codec.field "U32BE" uint32be (fun a -> a.ai_u32be);
        f_ints_u64be;
      ]

let all_ints_struct = Codec.to_struct all_ints_codec
let all_ints_size = Codec.wire_size all_ints_codec

let all_ints_default =
  {
    ai_u8 = 0xFF;
    ai_u16 = 0x1234;
    ai_u16be = 0x5678;
    ai_u32 = 0xDEADBEEF;
    ai_u32be = 0xCAFEBABE;
    ai_u64be = 0x0102030405060708L;
  }

let all_ints_data n =
  Array.init n (fun i ->
      let b = Bytes.create all_ints_size in
      Bytes.set_uint8 b 0 (i mod 256);
      Bytes.set_uint16_le b 1 (i mod 65536);
      Bytes.set_uint16_be b 3 (i * 7 mod 65536);
      Bytes.set_int32_le b 5 (Int32.of_int (i * 13));
      Bytes.set_int32_be b 9 (Int32.of_int (i * 17));
      Bytes.set_int64_be b 13 (Int64.of_int (i * 31));
      b)

(* ── 3. Bitfield8: 3+5 bits in bf_uint8 = 1 byte ── *)

type bf8 = { bf8_tag : int; bf8_value : int }

let f_bf8_value =
  Codec.field "Value" (bits ~width:5 bf_uint8) (fun b -> b.bf8_value)

let bf8_codec =
  Codec.view "Bitfield8"
    (fun tag value -> { bf8_tag = tag; bf8_value = value })
    Codec.
      [
        Codec.field "Tag" (bits ~width:3 bf_uint8) (fun b -> b.bf8_tag);
        f_bf8_value;
      ]

let bf8_struct = Codec.to_struct bf8_codec
let bf8_size = Codec.wire_size bf8_codec
let bf8_default = { bf8_tag = 5; bf8_value = 19 }

let bf8_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf8_size in
      let w = ((i mod 8) lsl 5) lor (i mod 32) in
      Bytes.set_uint8 b 0 w;
      b)

(* ── 4. Bitfield16: 1+4+11 bits in bf_uint16be = 2 bytes ── *)

type bf16 = { bf16_flag : int; bf16_type : int; bf16_id : int }

let f_bf16_id =
  Codec.field "Id" (bits ~width:11 bf_uint16be) (fun b -> b.bf16_id)

let bf16_codec =
  Codec.view "Bitfield16"
    (fun flag type_ id -> { bf16_flag = flag; bf16_type = type_; bf16_id = id })
    Codec.
      [
        Codec.field "Flag" (bits ~width:1 bf_uint16be) (fun b -> b.bf16_flag);
        Codec.field "Type" (bits ~width:4 bf_uint16be) (fun b -> b.bf16_type);
        f_bf16_id;
      ]

let bf16_struct = Codec.to_struct bf16_codec
let bf16_size = Codec.wire_size bf16_codec
let bf16_default = { bf16_flag = 1; bf16_type = 9; bf16_id = 1023 }

let bf16_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf16_size in
      let w = ((i mod 2) lsl 15) lor ((i * 3 mod 16) lsl 11) lor (i mod 2048) in
      Bytes.set_uint16_be b 0 w;
      b)

(* ── 5. Bitfield32: 4+6+14+8 bits in bf_uint32be = 4 bytes ── *)

type bf32 = {
  bf32_flags : int;
  bf32_chan : int;
  bf32_seq : int;
  bf32_pri : int;
}

let f_bf32_pri =
  Codec.field "Priority" (bits ~width:8 bf_uint32be) (fun b -> b.bf32_pri)

let bf32_codec =
  Codec.view "Bitfield32"
    (fun flags chan seq pri ->
      { bf32_flags = flags; bf32_chan = chan; bf32_seq = seq; bf32_pri = pri })
    Codec.
      [
        Codec.field "Flags" (bits ~width:4 bf_uint32be) (fun b -> b.bf32_flags);
        Codec.field "Channel" (bits ~width:6 bf_uint32be) (fun b -> b.bf32_chan);
        Codec.field "Seq" (bits ~width:14 bf_uint32be) (fun b -> b.bf32_seq);
        f_bf32_pri;
      ]

let bf32_struct = Codec.to_struct bf32_codec
let bf32_size = Codec.wire_size bf32_codec

let bf32_default =
  { bf32_flags = 5; bf32_chan = 26; bf32_seq = 4660; bf32_pri = 171 }

let bf32_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf32_size in
      let w =
        ((i mod 16) lsl 28)
        lor ((i * 3 mod 64) lsl 22)
        lor ((i * 17 mod 16384) lsl 8)
        lor (i mod 256)
      in
      Bytes.set_int32_be b 0 (Int32.of_int w);
      b)

(* ── 6. BoolFields: bool(1bit)+bool(1bit)+6bits+uint8 = 2 bytes ── *)

type bool_fields = {
  bl_active : bool;
  bl_valid : bool;
  bl_mode : int;
  bl_code : int;
}

let f_bool_active =
  Codec.field "Active" (bool (bits ~width:1 bf_uint8)) (fun b -> b.bl_active)

let bool_fields_codec =
  Codec.view "BoolFields"
    (fun active valid mode code ->
      { bl_active = active; bl_valid = valid; bl_mode = mode; bl_code = code })
    Codec.
      [
        f_bool_active;
        Codec.field "Valid"
          (bool (bits ~width:1 bf_uint8))
          (fun b -> b.bl_valid);
        Codec.field "Mode" (bits ~width:6 bf_uint8) (fun b -> b.bl_mode);
        Codec.field "Code" uint8 (fun b -> b.bl_code);
      ]

let bool_fields_struct = Codec.to_struct bool_fields_codec
let bool_fields_size = Codec.wire_size bool_fields_codec

let bool_fields_default =
  { bl_active = true; bl_valid = false; bl_mode = 7; bl_code = 0xAB }

let bool_fields_data n =
  Array.init n (fun i ->
      let b = Bytes.create bool_fields_size in
      let w = ((i mod 2) lsl 7) lor (((i + 1) mod 2) lsl 6) lor (i mod 64) in
      Bytes.set_uint8 b 0 w;
      Bytes.set_uint8 b 1 (i mod 256);
      b)

(* ── 7. Large mixed: u32be+u8+u8+u16be+u8+u8+u16be+u16be+u32be+u64be = 26 bytes ── *)

type large_mixed = {
  lg_sync : int;
  lg_version : int;
  lg_type : int;
  lg_spacecraft : int;
  lg_vcid : int;
  lg_count : int;
  lg_offset : int;
  lg_length : int;
  lg_crc : int;
  lg_timestamp : int64;
}

let f_mixed_timestamp =
  Codec.field "Timestamp" uint64be (fun l -> l.lg_timestamp)

let large_mixed_codec =
  Codec.view "LargeMixed"
    (fun sync version type_ spacecraft vcid count offset length crc timestamp ->
      {
        lg_sync = sync;
        lg_version = version;
        lg_type = type_;
        lg_spacecraft = spacecraft;
        lg_vcid = vcid;
        lg_count = count;
        lg_offset = offset;
        lg_length = length;
        lg_crc = crc;
        lg_timestamp = timestamp;
      })
    Codec.
      [
        Codec.field "SyncMarker" uint32be (fun l -> l.lg_sync);
        Codec.field "Version" uint8 (fun l -> l.lg_version);
        Codec.field "Type" uint8 (fun l -> l.lg_type);
        Codec.field "SpacecraftId" uint16be (fun l -> l.lg_spacecraft);
        Codec.field "VCID" uint8 (fun l -> l.lg_vcid);
        Codec.field "FrameCount" uint8 (fun l -> l.lg_count);
        Codec.field "DataOffset" uint16be (fun l -> l.lg_offset);
        Codec.field "DataLength" uint16be (fun l -> l.lg_length);
        Codec.field "CRC" uint32be (fun l -> l.lg_crc);
        f_mixed_timestamp;
      ]

let large_mixed_struct = Codec.to_struct large_mixed_codec
let large_mixed_size = Codec.wire_size large_mixed_codec

let large_mixed_default =
  {
    lg_sync = 0x1ACFFC1D;
    lg_version = 2;
    lg_type = 0;
    lg_spacecraft = 0x01FF;
    lg_vcid = 3;
    lg_count = 66;
    lg_offset = 16;
    lg_length = 1024;
    lg_crc = 0xDEADBEEF;
    lg_timestamp = 0x0102030405060708L;
  }

let large_mixed_data n =
  Array.init n (fun i ->
      let b = Bytes.create large_mixed_size in
      Bytes.set_int32_be b 0 0x1ACFFC1Dl;
      Bytes.set_uint8 b 4 (i mod 4);
      Bytes.set_uint8 b 5 (i mod 3);
      Bytes.set_uint16_be b 6 (i mod 1024);
      Bytes.set_uint8 b 8 (i mod 8);
      Bytes.set_uint8 b 9 (i mod 256);
      Bytes.set_uint16_be b 10 16;
      Bytes.set_uint16_be b 12 (i mod 2048);
      Bytes.set_int32_be b 14 (Int32.of_int (i * 0x1234));
      Bytes.set_int64_be b 18 (Int64.of_int (i * 1_000_000));
      b)

(* ── 8. Mapped: user-defined decode/encode via map combinator = 2 bytes ──
   The map decode function runs on every Codec.get call, adding a function
   call to the hot path. On the C side, map is invisible (3D sees the inner
   type), so this measures the OCaml mapping overhead vs bare C validation. *)

type priority = Low | Medium | High | Critical

let priority_of_int = function
  | 0 -> Low
  | 1 -> Medium
  | 2 -> High
  | _ -> Critical

let int_of_priority = function
  | Low -> 0
  | Medium -> 1
  | High -> 2
  | Critical -> 3

type mapped = { mp_priority : priority; mp_value : int }

let f_mp_priority =
  Codec.field "Priority" (map priority_of_int int_of_priority uint8) (fun m ->
      m.mp_priority)

let f_mp_value = Codec.field "Value" uint8 (fun m -> m.mp_value)

let mapped_codec =
  Codec.view "Mapped"
    (fun pri value -> { mp_priority = pri; mp_value = value })
    Codec.[ f_mp_priority; f_mp_value ]

let mapped_struct = Codec.to_struct mapped_codec
let mapped_size = Codec.wire_size mapped_codec
let mapped_default = { mp_priority = High; mp_value = 42 }

let mapped_data n =
  let buf = Bytes.create (n * mapped_size) in
  for i = 0 to n - 1 do
    Codec.encode mapped_codec
      { mp_priority = priority_of_int (i mod 4); mp_value = i mod 256 }
      buf (i * mapped_size)
  done;
  buf

(* ── 9. CasesDemo: variant dispatch via cases combinator = 1 byte ──
   The cases combinator uses an array lookup on decode (Codec.get) and a
   linear scan on encode (Codec.set). On the C side, it's just a 1-bit
   bitfield — the variant mapping is OCaml-only. *)

type ptype = Telemetry | Telecommand
type cases_demo = { cd_type : ptype; cd_id : int }

let f_cd_type =
  Codec.field "PacketType"
    (cases [ Telemetry; Telecommand ] (bits ~width:1 bf_uint8))
    (fun c -> c.cd_type)

let f_cd_id = Codec.field "Id" (bits ~width:7 bf_uint8) (fun c -> c.cd_id)

let cases_demo_codec =
  Codec.view "CasesDemo"
    (fun ptype id -> { cd_type = ptype; cd_id = id })
    Codec.[ f_cd_type; f_cd_id ]

let cases_demo_struct = Codec.to_struct cases_demo_codec
let cases_demo_size = Codec.wire_size cases_demo_codec
let cases_demo_default = { cd_type = Telemetry; cd_id = 42 }

let cases_demo_data n =
  let buf = Bytes.create (n * cases_demo_size) in
  for i = 0 to n - 1 do
    Codec.encode cases_demo_codec
      {
        cd_type = (if i mod 2 = 0 then Telemetry else Telecommand);
        cd_id = i mod 128;
      }
      buf (i * cases_demo_size)
  done;
  buf

(* ── 10. EnumDemo: enum + map for OCaml variant types = 2 bytes ──
   Combines enum (3D validation: rejects values outside {0,1,2,3}) with map
   (OCaml variant decode/encode). EverParse C validates enum membership;
   Codec.get calls the map decode on every read. *)

type status = [ `Ok | `Warn | `Err | `Crit ]

let status_of_int = function 0 -> `Ok | 1 -> `Warn | 2 -> `Err | _ -> `Crit
let int_of_status = function `Ok -> 0 | `Warn -> 1 | `Err -> 2 | `Crit -> 3
let status_3d_cases = [ ("OK", 0); ("WARN", 1); ("ERR", 2); ("CRIT", 3) ]

type enum_demo = { en_status : status; en_code : int }

let f_en_status =
  Codec.field "StatusCode"
    (map status_of_int int_of_status (enum "Status" status_3d_cases uint8))
    (fun e -> e.en_status)

let f_en_code = Codec.field "Code" uint8 (fun e -> e.en_code)

let enum_demo_codec =
  Codec.view "EnumDemo"
    (fun status code -> { en_status = status; en_code = code })
    Codec.[ f_en_status; f_en_code ]

let enum_demo_struct = Codec.to_struct enum_demo_codec
let enum_demo_size = Codec.wire_size enum_demo_codec
let enum_demo_default = { en_status = `Ok; en_code = 42 }

let enum_demo_data n =
  let buf = Bytes.create (n * enum_demo_size) in
  let statuses = [| `Ok; `Warn; `Err; `Crit |] in
  for i = 0 to n - 1 do
    Codec.encode enum_demo_codec
      { en_status = statuses.(i mod 4); en_code = i mod 256 }
      buf (i * enum_demo_size)
  done;
  buf

(* ── 11. Constrained: where clause, validation on C side only = 2 bytes ──
   The where constraint generates a check in the EverParse C validator
   (Version must be 0) but Codec.get strips the constraint entirely.
   This measures C constraint-checking overhead vs OCaml unchecked read. *)

type constrained = { co_version : int; co_data : int }

let f_co_version =
  Codec.field "Version"
    (where Expr.(ref "Version" = int 0) uint8)
    (fun c -> c.co_version)

let f_co_data = Codec.field "Data" uint8 (fun c -> c.co_data)

let constrained_codec =
  Codec.view "Constrained"
    (fun version data -> { co_version = version; co_data = data })
    Codec.[ f_co_version; f_co_data ]

let constrained_struct = Codec.to_struct constrained_codec
let constrained_size = Codec.wire_size constrained_codec
let constrained_default = { co_version = 0; co_data = 42 }

let constrained_data n =
  let buf = Bytes.create (n * constrained_size) in
  for i = 0 to n - 1 do
    Codec.encode constrained_codec
      { co_version = 0; co_data = i mod 256 }
      buf (i * constrained_size)
  done;
  buf
