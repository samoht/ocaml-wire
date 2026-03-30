/* Wire field extraction callbacks for EverParse output types.

   WireSet* functions store extracted field values into a C int64_t array
   at the field's index. When fields is NULL (validation-only), no-op.

   This file is NOT generated — it is part of the wire.stubs library. */

#include <stdint.h>

#ifndef WIRECTX_DEFINED
#define WIRECTX_DEFINED
typedef struct { int64_t *fields; } WIRECTX;
#endif

void WireSetU8(WIRECTX *ctx, uint32_t idx, uint8_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU16(WIRECTX *ctx, uint32_t idx, uint16_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU16be(WIRECTX *ctx, uint32_t idx, uint16_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU32(WIRECTX *ctx, uint32_t idx, uint32_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU32be(WIRECTX *ctx, uint32_t idx, uint32_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU64(WIRECTX *ctx, uint32_t idx, uint64_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetU64be(WIRECTX *ctx, uint32_t idx, uint64_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}

void WireSetBytes(WIRECTX *ctx, uint32_t idx, uint32_t v) {
  if (ctx->fields) ctx->fields[idx] = (int64_t)v;
}
