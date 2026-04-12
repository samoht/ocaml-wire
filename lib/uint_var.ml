let read_be buf off size =
  let v = ref 0 in
  for i = 0 to size - 1 do
    v := (!v lsl 8) lor Bytes.get_uint8 buf (off + i)
  done;
  !v

let read_le buf off size =
  let v = ref 0 in
  for i = size - 1 downto 0 do
    v := (!v lsl 8) lor Bytes.get_uint8 buf (off + i)
  done;
  !v

let read endian buf off size =
  match (endian : Types.endian) with
  | Big -> read_be buf off size
  | Little -> read_le buf off size

let write_be buf off size v =
  let v = ref v in
  for i = size - 1 downto 0 do
    Bytes.set_uint8 buf (off + i) (!v land 0xFF);
    v := !v lsr 8
  done

let write_le buf off size v =
  let v = ref v in
  for i = 0 to size - 1 do
    Bytes.set_uint8 buf (off + i) (!v land 0xFF);
    v := !v lsr 8
  done

let write endian buf off size v =
  match (endian : Types.endian) with
  | Big -> write_be buf off size v
  | Little -> write_le buf off size v
