/* Wire field extraction context for EverParse output types.

   This header is NOT generated — it is part of the wire.stubs library.
   Include it from ExternalTypedefs.h or any C file that calls EverParse
   validators with output types. */

#ifndef WIRECTX_DEFINED
#define WIRECTX_DEFINED

#include <stdint.h>

typedef struct { int64_t *fields; } WIRECTX;

void WireSetU8(WIRECTX *ctx, uint32_t idx, uint8_t v);
void WireSetU16(WIRECTX *ctx, uint32_t idx, uint16_t v);
void WireSetU16be(WIRECTX *ctx, uint32_t idx, uint16_t v);
void WireSetU32(WIRECTX *ctx, uint32_t idx, uint32_t v);
void WireSetU32be(WIRECTX *ctx, uint32_t idx, uint32_t v);
void WireSetU64(WIRECTX *ctx, uint32_t idx, uint64_t v);
void WireSetU64be(WIRECTX *ctx, uint32_t idx, uint64_t v);
void WireSetBytes(WIRECTX *ctx, uint32_t idx, uint32_t v);

#endif
