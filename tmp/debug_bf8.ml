type constrained_raw = { data : int }

let () =
  let data = Demo.constrained_data 5 in
  let size = Demo.constrained_size in
  let items =
    Array.init
      (Bytes.length data / size)
      (fun i -> Bytes.sub data (i * size) size)
  in
  Array.iteri
    (fun i buf ->
      let v0 = Bytes.get_uint8 buf 0 in
      let v1 = Bytes.get_uint8 buf 1 in
      let ocaml_v =
        (Wire.Staged.unstage
           (Wire.Codec.get Demo.constrained_codec Demo.bf_co_data))
          buf 0
      in
      let c_ok = C_stubs.constrained_check buf in
      let (c_raw : constrained_raw) = C_stubs.constrained_parse buf in
      Printf.printf "item %d: [0x%02x 0x%02x] ocaml=%d c_ok=%b c=%d\n" i v0 v1
        ocaml_v c_ok c_raw.data)
    items
