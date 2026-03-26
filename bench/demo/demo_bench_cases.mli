type dataset = { items : bytes array; packed : bytes; n_items : int }

type id =
  | Minimal
  | All_ints
  | Large_mixed
  | Bitfield8
  | Bitfield16
  | Bitfield32
  | Bool_fields
  | Clcw_report
  | Space_packet_apid
  | Ipv4_src
  | Tcp_dst_port
  | Tcp_syn
  | Mapped_priority
  | Cases_type
  | Enum_status
  | Constrained_data

type read_case =
  | Read_case : {
      id : id;
      label : string;
      size : int;
      dataset : dataset;
      struct_ : Wire.Everparse.struct_;
      codec : 'r Wire.Codec.t;
      get : bytes -> int -> 'a;
      set : bytes -> int -> 'a -> unit;
      write_template : bytes;
      write_offset : int;
      write_value : 'a;
      equal : 'a -> 'a -> bool;
      mutable c_parse : (bytes -> 'a) option;
    }
      -> read_case

type write_case = { label : string; run : unit -> unit; verify : unit -> unit }

val projection_cases : read_case list
val projection_structs : Wire.Everparse.struct_ list
val read_benchmark_cases : read_case list
val write_benchmark_cases : write_case list
