/* Shared utilities for C benchmark loops. */

#ifndef BENCH_COMMON_H
#define BENCH_COMMON_H

#include <stdint.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

static void bench_err(const char *t, const char *f, const char *r,
  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {
  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;
}

#endif
