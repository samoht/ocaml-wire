(** TCP/IP protocol header codecs using Wire's zero-copy API.

    Ethernet (14B) -> IPv4 (20B) -> TCP (20B) or UDP (8B). All payload fields
    use [byte_slice] for zero-copy sub-slicing. *)

(** {2 Ethernet II} *)

type ethernet

val ethernet_codec : ethernet Wire.Codec.t
val ethernet_size : int
val ethernet_payload_size : int
val f_eth_ethertype : (int, ethernet) Wire.Codec.field
val f_eth_payload : (Bytesrw.Bytes.Slice.t, ethernet) Wire.Codec.field

(** {2 IPv4} *)

type ipv4

val ipv4_codec : ipv4 Wire.Codec.t
val ipv4_size : int
val ipv4_payload_size : int
val f_ip_protocol : (int, ipv4) Wire.Codec.field
val f_ip_src : (int, ipv4) Wire.Codec.field
val f_ip_dst : (int, ipv4) Wire.Codec.field
val f_ip_payload : (Bytesrw.Bytes.Slice.t, ipv4) Wire.Codec.field

(** {2 TCP} *)

type tcp

val tcp_codec : tcp Wire.Codec.t
val tcp_size : int
val f_tcp_src_port : (int, tcp) Wire.Codec.field
val f_tcp_dst_port : (int, tcp) Wire.Codec.field
val f_tcp_syn : (bool, tcp) Wire.Codec.field
val f_tcp_ack : (bool, tcp) Wire.Codec.field

(** {2 UDP} *)

type udp

val udp_codec : udp Wire.Codec.t
val udp_size : int
val f_udp_src_port : (int, udp) Wire.Codec.field
val f_udp_dst_port : (int, udp) Wire.Codec.field
val f_udp_length : (int, udp) Wire.Codec.field

(** {2 Utilities} *)

val pp_ipv4_addr : Format.formatter -> int -> unit
val ipv4_addr : int -> int -> int -> int -> int

(** {2 Data generators} *)

val tcp_frame_data : int -> bytes array
val udp_frame_data : int -> bytes array

(** {2 Schema registry} *)

type 'a schema = {
  name : string;
  codec : 'a Wire.Codec.t;
  struct_ : Wire.struct_;
  size : int;
  decode : bytes -> int -> 'a;
}

type any_schema = Any : 'a schema -> any_schema

val all_schemas : any_schema list
val all_structs : Wire.struct_ list
