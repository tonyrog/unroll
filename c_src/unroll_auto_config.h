#ifndef __UNROLL_AUTO_CONFIG__
#define __UNROLL_AUTO_CONFIG__

#include <stdint.h>

#if (__SIZEOF_POINTER__ == 4) && defined(__SIZEOF_LONG_LONG__) && (__SIZEOF_LONG_LONG__ == 8)
#define UINT_T  uint32_t
#define UINTD_T uint64_t
#define UINTH_T uint16_t
#elif (__SIZEOF_POINTER__ == 4)
#define UINT_T  uint32_t
#define UINTH_T uint16_t
#elif (__SIZEOF_POINTER__ == 8)
#define UINT_T  uint64_t
#ifdef __SIZEOF_INT128__
#define UINTD_T __uint128_t
#endif
#define UINTH_T uint32_t
#else
#error "cannot determine machine word size"
#endif

#endif
