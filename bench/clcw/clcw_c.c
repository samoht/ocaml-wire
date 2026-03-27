/* CLCW polling loop — application logic with EverParse field extraction.

   Uses EverParse-generated CLCWValidateCLCW to extract bitfields via WireSet
   callbacks into a C array. Application logic (anomaly detection) uses the
   extracted values. No hand-written bitfield manipulation. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <stdint.h>
#include <time.h>

/* WIRECTX + WireSet — field extraction via EverParse output types.
   v_ptr mode: OCaml continuation callbacks (unused here).
   fields mode: store extracted values into a C int64_t array. */
#ifndef WIRECTX_DEFINED
#define WIRECTX_DEFINED
typedef struct { value *v_ptr; int64_t *fields; } WIRECTX;
#endif

#define WIRESET_IMPL(NAME, CTYPE) \
  void NAME(WIRECTX *ctx, uint32_t idx, CTYPE v) { \
    if (ctx->fields) ctx->fields[idx] = (int64_t)v; \
  }
WIRESET_IMPL(WireSetU8, uint8_t)
WIRESET_IMPL(WireSetU16, uint16_t)
WIRESET_IMPL(WireSetU16be, uint16_t)
WIRESET_IMPL(WireSetU32, uint32_t)
WIRESET_IMPL(WireSetU32be, uint32_t)
WIRESET_IMPL(WireSetU64, uint64_t)
WIRESET_IMPL(WireSetU64be, uint64_t)

/* EverParse generated headers and sources */
#include "EverParse.h"
#include "CLCW.h"
#include "CLCW.c"

static void clcw_err(const char *t, const char *f, const char *r,
  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {
  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;
}

/* Field indices in the CLCW output struct (declaration order of named fields).
   These must match the order in Space.clcw_codec. */
enum {
  CLCW_CONTROLWORDTYPE = 0,
  CLCW_CLCWVERSION,
  CLCW_STATUSFIELD,
  CLCW_COPINEFFECT,
  CLCW_VCID,
  CLCW_NORF,
  CLCW_NOBITLOCK,
  CLCW_LOCKOUT,
  CLCW_WAIT,
  CLCW_RETRANSMIT,
  CLCW_FARMBCOUNTER,
  CLCW_REPORTVALUE,
  CLCW_N_FIELDS
};

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

static int count_anomalies(uint8_t *buf, int n_words, int n_iters) {
  int word_size = 4;
  int anomalies = 0;
  int expected_seq = 0;
  int64_t fields[CLCW_N_FIELDS];
  WIRECTX ctx = { NULL, fields };

  for (int i = 0; i < n_iters; i++) {
    uint8_t *p = buf + (i % n_words) * word_size;
    ClcwValidateClcw(&ctx, NULL, clcw_err, p, word_size, 0);

    int lockout    = (int)fields[CLCW_LOCKOUT];
    int wait_      = (int)fields[CLCW_WAIT];
    int retransmit = (int)fields[CLCW_RETRANSMIT];
    int report     = (int)fields[CLCW_REPORTVALUE];
    int expected_report = expected_seq & 0xFF;
    if (lockout || wait_ || retransmit || report != expected_report)
      anomalies++;
    expected_seq = (report + 1) & 0xFF;
  }
  return anomalies;
}

CAMLprim value c_clcw_poll(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  int word_size = 4;
  int n_words = buf_len / word_size;
  int64_t t0 = now_ns();
  volatile int anomalies = count_anomalies(buf, n_words, n);
  (void)anomalies;
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

CAMLprim value c_clcw_poll_result(value v_buf, value v_off) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int word_size = 4;
  int n_words = buf_len / word_size;
  return Val_int(count_anomalies(buf, n_words, n_words));
}
