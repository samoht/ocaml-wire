/* TM frame reassembly — application logic with EverParse field extraction.

   Uses EverParse-generated TmframeValidateTmframe and
   SpacePacketValidateSpacePacket to extract fields via WireSet callbacks
   into C arrays. Application logic (checksum computation) uses the extracted
   values. No hand-written bitfield manipulation. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>

#include "wire_setters.h"

/* EverParse generated headers (implementation linked from c_stubs_c) */
#include "EverParse.h"
#include "TMFrame.h"
#include "SpacePacket.h"

#include "bench_common.h"

/* TMFrame field indices (declaration order) */
enum {
  TF_VERSION = 0, TF_SCID, TF_VCID, TF_OCFFLAG,
  TF_MCCOUNT, TF_VCCOUNT, TF_SECHDRFLAG, TF_SYNCFLAG,
  TF_PACKETORDER, TF_SEGLENID, TF_FIRSTHDRPTR,
  TF_N_FIELDS
};

/* SpacePacket field indices (declaration order) */
enum {
  SP_VERSION = 0, SP_TYPE, SP_SECHDRFLAG, SP_APID,
  SP_SEQFLAGS, SP_SEQCOUNT, SP_DATALENGTH,
  SP_N_FIELDS
};

/* Must match gateway_bench.ml — derived from Space.tm_frame_codec / Space.packet_codec */
static const int CADU_SIZE = 1115;
static const int TM_HDR = 6;       /* Wire.Codec.wire_size Space.tm_frame_codec */
static const int PKT_SIZE = 70;    /* sp_hdr (6) + pkt_payload (64) */
static const int DATA_FIELD_SIZE = 1115 - 6;  /* CADU_SIZE - TM_HDR */

static const uint64_t CHECKSUM_INIT = 0xCBF29CE484222325ULL;
static const uint64_t CHECKSUM_PRIME = 0x100000001B3ULL;

static inline uint64_t hash_int(uint64_t state, int value) {
  return (state ^ (uint64_t)value) * CHECKSUM_PRIME;
}

static void walk_frame(uint8_t *frame, int tm_hdr, int pkt_size,
                        int data_field_size, uint64_t *checksum) {
  int64_t tf[TF_N_FIELDS];
  WIRECTX tf_ctx = { tf };
  TmframeValidateTmframe(&tf_ctx, NULL, bench_err, frame, tm_hdr, 0);

  int vcid = (int)tf[TF_VCID];
  int fhp = (int)tf[TF_FIRSTHDRPTR];
  if (checksum != NULL) {
    *checksum = hash_int(hash_int(*checksum, vcid), fhp);
  } else {
    volatile int keep = vcid + fhp;
    (void)keep;
  }

  int64_t sp[SP_N_FIELDS];
  WIRECTX sp_ctx = { sp };
  int sp_hdr = 6;
  int off = tm_hdr + fhp;
  while (off + pkt_size <= tm_hdr + data_field_size) {
    SpacePacketValidateSpacePacket(&sp_ctx, NULL, bench_err,
        frame + off, sp_hdr, 0);
    int apid = (int)sp[SP_APID];
    int seq = (int)sp[SP_SEQCOUNT];
    if (checksum != NULL) {
      *checksum = hash_int(hash_int(*checksum, apid), seq);
    } else {
      volatile int keep = apid + seq;
      (void)keep;
    }
    off += pkt_size;
  }
}

CAMLprim value c_tm_reassemble(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  int n_frames = buf_len / CADU_SIZE;
  int64_t t0 = now_ns();
  for (int i = 0; i < n; i++) {
    uint8_t *frame = buf + (i % n_frames) * CADU_SIZE;
    walk_frame(frame, TM_HDR, PKT_SIZE, DATA_FIELD_SIZE, NULL);
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

CAMLprim value c_tm_reassemble_checksum(value v_buf, value v_off) {
  CAMLparam2(v_buf, v_off);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n_frames = buf_len / CADU_SIZE;
  uint64_t checksum = CHECKSUM_INIT;
  for (int i = 0; i < n_frames; i++) {
    uint8_t *frame = buf + i * CADU_SIZE;
    walk_frame(frame, TM_HDR, PKT_SIZE, DATA_FIELD_SIZE, &checksum);
  }
  CAMLreturn(caml_copy_int64((int64_t)checksum));
}
