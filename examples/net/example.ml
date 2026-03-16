(** Demonstrates zero-copy TCP/IP header parsing with Wire.

    Three nested protocol layers accessed through sub-slices — no intermediate
    record allocation, no buffer copies. *)

open Wire
open Net
module Bs = Bytesrw.Bytes.Slice

let () =
  let buf = (tcp_frame_data 1).(0) in
  let frame = Bs.make buf ~first:0 ~length:(Bytes.length buf) in

  Fmt.pr "TCP/IP Zero-Copy Header Parsing with Wire\n";
  Fmt.pr "==========================================\n\n";
  Fmt.pr "Frame: %d bytes in a single buffer\n\n" (Bytes.length buf);

  (* Layer 1: Ethernet — read ethertype, get IPv4 sub-slice *)
  let etype = Codec.get ethernet_codec f_eth_ethertype frame in
  let ip_slice = Codec.get ethernet_codec f_eth_payload frame in
  Fmt.pr "Ethernet: ethertype=0x%04X payload=[%d..%d)\n" etype
    (Bs.first ip_slice)
    (Bs.first ip_slice + Bs.length ip_slice);

  (* Layer 2: IPv4 — read fields from the sub-slice, get TCP sub-slice *)
  let protocol = Codec.get ipv4_codec f_ip_protocol ip_slice in
  let src = Codec.get ipv4_codec f_ip_src ip_slice in
  let dst = Codec.get ipv4_codec f_ip_dst ip_slice in
  let tcp_slice = Codec.get ipv4_codec f_ip_payload ip_slice in
  Fmt.pr "IPv4:     protocol=%d src=%a dst=%a payload=[%d..%d)\n" protocol
    pp_ipv4_addr src pp_ipv4_addr dst (Bs.first tcp_slice)
    (Bs.first tcp_slice + Bs.length tcp_slice);

  (* Layer 3: TCP — read fields from the sub-slice *)
  let src_port = Codec.get tcp_codec f_tcp_src_port tcp_slice in
  let dst_port = Codec.get tcp_codec f_tcp_dst_port tcp_slice in
  let syn = Codec.get tcp_codec f_tcp_syn tcp_slice in
  let ack = Codec.get tcp_codec f_tcp_ack tcp_slice in
  Fmt.pr "TCP:      %d -> %d SYN=%b ACK=%b\n\n" src_port dst_port syn ack;

  (* In-place mutation: change destination port *)
  Codec.set tcp_codec f_tcp_dst_port tcp_slice 8080;
  Fmt.pr "After set: TCP dst_port=%d\n"
    (Codec.get tcp_codec f_tcp_dst_port tcp_slice);

  (* Verify zero-copy: all slices share the same buffer *)
  Fmt.pr "\nAll slices share the same underlying buffer: %b\n"
    (Bs.bytes frame == Bs.bytes ip_slice
    && Bs.bytes ip_slice == Bs.bytes tcp_slice);
  Fmt.pr
    "3 protocol layers parsed from a single %dB buffer, zero allocation for \
     int fields.\n"
    (Bytes.length buf)
