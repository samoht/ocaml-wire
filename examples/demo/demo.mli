(** Synthetic benchmark schemas exercising all Wire API constructs. *)

(** {1 Minimal (1 byte)} *)

type minimal = { m_value : int }

val minimal_codec : minimal Wire.Codec.t
val minimal_struct : Wire.struct_
val minimal_size : int
val minimal_default : minimal
val minimal_data : int -> bytes array

(** {1 AllInts (21 bytes)} *)

type all_ints = {
  ai_u8 : int;
  ai_u16 : int;
  ai_u16be : int;
  ai_u32 : int;
  ai_u32be : int;
  ai_u64be : int64;
}

val all_ints_codec : all_ints Wire.Codec.t
val all_ints_struct : Wire.struct_
val all_ints_size : int
val all_ints_default : all_ints
val all_ints_data : int -> bytes array

(** {1 Bitfield8 (1 byte)} *)

type bf8 = { bf8_tag : int; bf8_value : int }

val bf8_codec : bf8 Wire.Codec.t
val bf8_struct : Wire.struct_
val bf8_size : int
val bf8_default : bf8
val bf8_data : int -> bytes array

(** {1 Bitfield16 (2 bytes)} *)

type bf16 = { bf16_flag : int; bf16_type : int; bf16_id : int }

val bf16_codec : bf16 Wire.Codec.t
val bf16_struct : Wire.struct_
val bf16_size : int
val bf16_default : bf16
val bf16_data : int -> bytes array

(** {1 Bitfield32 (4 bytes)} *)

type bf32 = {
  bf32_flags : int;
  bf32_chan : int;
  bf32_seq : int;
  bf32_pri : int;
}

val bf32_codec : bf32 Wire.Codec.t
val bf32_struct : Wire.struct_
val bf32_size : int
val bf32_default : bf32
val bf32_data : int -> bytes array

(** {1 BoolFields (2 bytes)} *)

type bool_fields = {
  bl_active : bool;
  bl_valid : bool;
  bl_mode : int;
  bl_code : int;
}

val bool_fields_codec : bool_fields Wire.Codec.t
val bool_fields_struct : Wire.struct_
val bool_fields_size : int
val bool_fields_default : bool_fields
val bool_fields_data : int -> bytes array

(** {1 LargeMixed (26 bytes)} *)

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

val large_mixed_codec : large_mixed Wire.Codec.t
val large_mixed_struct : Wire.struct_
val large_mixed_size : int
val large_mixed_default : large_mixed
val large_mixed_data : int -> bytes array
