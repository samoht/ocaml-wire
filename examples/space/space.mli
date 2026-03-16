(** CCSDS space protocol codecs. *)

(** {2 CLCW} *)

type clcw

val clcw_codec : clcw Wire.Codec.t
val clcw_struct : Wire.struct_
val clcw_size : int
val clcw_default : clcw
val clcw_data : int -> bytes array
val cw_report : (int, clcw) Wire.Codec.field

(** {2 SpacePacket} *)

type space_packet

val space_packet_codec : space_packet Wire.Codec.t
val space_packet_struct : Wire.struct_
val space_packet_size : int
val space_packet_default : space_packet
val space_packet_data : int -> bytes array

(** {2 TMFrame} *)

type tm_frame

val tm_frame_codec : tm_frame Wire.Codec.t
val tm_frame_struct : Wire.struct_
val tm_frame_size : int
val tm_frame_default : tm_frame
val tm_frame_data : int -> bytes array

(** {2 Nested protocol} *)

type inner_cmd
type outer_hdr

val inner_cmd_codec : inner_cmd Wire.Codec.t
val inner_cmd_size : int
val f_cmd_id : (int, inner_cmd) Wire.Codec.field
val f_cmd_seq : (int, inner_cmd) Wire.Codec.field
val outer_hdr_codec : outer_hdr Wire.Codec.t
val outer_hdr_size : int
val f_oh_length : (int, outer_hdr) Wire.Codec.field
val f_oh_payload : (Bytesrw.Bytes.Slice.t, outer_hdr) Wire.Codec.field
val nested_data : int -> bytes array
