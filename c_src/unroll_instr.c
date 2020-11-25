// inline instructions for unroll expansion

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef TEST

#define UINT_T  uint16_t
// #define UINTD_T uint32_t
#define UINTH_T uint8_t

#else

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

#define bit_sizeof(T) (sizeof(T)*8)

static inline void move (UINT_T src, UINT_T* dst)
{
    *dst = src;
}

static inline void add0(UINT_T src1, UINT_T src2, UINT_T* sum)
{
    *sum = src1 + src2;
}

static inline void add(UINT_T src1, UINT_T src2, UINT_T* co, UINT_T* sum)
{
    src1 += src2;
    *sum = src1;
    *co = (src1 < src2);
}

static inline void addc(UINT_T src1, UINT_T src2, UINT_T ci, UINT_T* co, UINT_T* sum)
{
    src1 += ci;
    ci = (src1 < ci);
    src1 += src2;
    *sum = src1;
    *co = ci + (src1 < src2);
}

static inline void sub0(UINT_T src1, UINT_T src2, UINT_T* diff)
{
    *diff = src1 - src2;
}

static inline void sub(UINT_T src1, UINT_T src2, UINT_T* bo, UINT_T* diff)
{
    src2 = src1 - src2;
    *diff = src2;
    *bo = (src2 > src1);
}

static inline void subb(UINT_T src1, UINT_T src2, UINT_T bi, UINT_T* bo, UINT_T* diff)
{
    src2 += bi;
    bi = (src2 < bi);
    src2 = src1 - src2;
    *bo = bi + (src2 > src1);
    *diff = src2;
}

static inline void mul0(UINT_T src1, UINT_T src2, UINT_T* p0)
{
    *p0 = src1 * src2;
}

#ifdef UINTD_T

#define HSHIFT bit_sizeof(UINT_T)
 
static inline void mul(UINT_T src1, UINT_T src2,
		       UINT_T* prod1, UINT_T* prod0)
{
    UINTD_T t = ((UINTD_T)src1)*src2;
    *prod0 = t >> HSHIFT;
    *prod1 = t;
}

static inline void mula(UINT_T src1, UINT_T src2, UINT_T a,
			UINT_T* prod1, UINT_T* prod0)
{
    UINTD_T t = ((UINTD_T)src1)*src2 + a;
    *prod0 = t >> HSHIFT;
    *prod1 = t;
}

static inline void sqra(UINT_T src1, UINT_T a,
			UINT_T* prod1, UINT_T* prod0)
{
    UINTD_T t = ((UINTD_T)src1)*src1 + a;
    *prod0 = t >> HSHIFT;
    *prod1 = t;
}

static inline void mulab(UINT_T src1, UINT_T src2, UINT_T a, UINT_T b,
			 UINT_T* prod1, UINT_T* prod0)
{
    UINTD_T t = ((UINTD_T)src1)*src2 + a + b;
    *prod0 = t >> HSHIFT;
    *prod1 = t;
}

#elif defined(UINTH_T)

#define HSHIFT bit_sizeof(UINTH_T)
#define LMASK  ((1 << HSHIFT)-1)
#define HMASK  (LMASK << HSHIFT)

static inline void mula(UINT_T src1, UINT_T src2, UINT_T a,
			UINT_T* prod1, UINT_T* prod0)
{
    UINTH_T a0 = src1;
    UINTH_T a1 = src1 >> HSHIFT;
    UINTH_T b0 = src2;
    UINTH_T b1 = src2 >> HSHIFT;
    UINT_T a0b0 = (UINT_T)a0*b0;
    UINT_T a0b1 = (UINT_T)a0*b1;
    UINT_T a1b0 = (UINT_T)a1*b0;
    UINT_T a1b1 = (UINT_T)a1*b1;
    UINT_T p0,p1,p2,c0;

    add(a0b0,a,&c0,&p0);
    add(c0<<HSHIFT,p0>>HSHIFT,&p2,&p1);
    add(p1,a0b1,&c0,&p1);
    p2 += c0;
    add(p1,a1b0,&c0,&p1);
    p2 += c0;
    add(p1,a1b1<<HSHIFT,&c0,&p1);
    p2 +=c0;
    add(a1b1,p2<<HSHIFT,&c0,&p2);
    *prod1 = (p2 & HMASK) | (p1 >> HSHIFT);
    *prod0 = (p1 << HSHIFT) | (p0 & LMASK);
}

static inline void sqra(UINT_T src1, UINT_T a,
			UINT_T* prod1, UINT_T* prod0)
{
    UINTH_T a0 = src1;
    UINTH_T a1 = src1 >> HSHIFT;
    UINT_T a0a0 = (UINT_T)a0*a0;
    UINT_T a0a1 = (UINT_T)a0*a1;
    UINT_T a1a1 = (UINT_T)a1*a1;
    UINT_T p0,p1,p2,c0;

    add(a0a0,a,&c0,&p0);
    add(c0<<HSHIFT,p0>>HSHIFT,&p2,&p1);
    add(p1,a0a1,&c0,&p1);
    p2 += c0;
    add(p1,a0a1,&c0,&p1);
    p2 += c0;
    add(p1,a1a1<<HSHIFT,&c0,&p1);
    p2 +=c0;
    add(a1a1,p2<<HSHIFT,&c0,&p2);
    *prod1 = (p2 & HMASK) | (p1 >> HSHIFT);
    *prod0 = (p1 << HSHIFT) | (p0 & LMASK);
}


static inline void mulab(UINT_T src1, UINT_T src2, UINT_T a, UINT_T b,
			 UINT_T* prod1, UINT_T* prod0)
{
    UINT_T c0;
    mula(src1, src2, a, prod1, prod0);
    add(*prod0, b, &c0, prod0);
    add(*prod1, c0, &c0, prod1);  // result is ignored
    assert(c0 == 0);
}

static inline void mul(UINT_T src1, UINT_T src2,
		       UINT_T* prod1, UINT_T* prod0)
{
    mula(src1, src2, 0, prod1, prod0);
}

#endif


#ifdef TEST

int main(int arc, char** argv)
{
    UINT_T u, v, r;
    UINT_T a, b;
    UINT_T y[2];

    // ADD0
    u = 65535;
    v = 65535;
    add0(u, v, &r);
    printf("add0: %d+%d=%d\n", u, v, r);

    u = 65535;
    v = 100;
    add0(u, v, &r);
    printf("add0: %d+%d=%d\n", u, v, r);

    // ADD
    u = 65535;
    v = 65535;
    add(u, v, &y[1], &y[0]);
    printf("add: %d+%d=(%d,%d)\n", u, v, y[1], y[0]);

    u = 65535;
    v = 100;
    add(u, v, &y[1], &y[0]);
    printf("add: %d+%d=(%d,%d)\n", u, v, y[1], y[0]);

    // ADDc
    u = 65535;
    v = 65535;
    addc(u, v, 1, &y[1], &y[0]);
    printf("addc: %d+%d+1=(%d,%d)\n", u, v, y[1], y[0]);

    u = 65535;
    v = 100;
    addc(u, v, 1, &y[1], &y[0]);
    printf("addc: %d+%d+1=(%d,%d)\n", u, v, y[1], y[0]);

    // SUB0
    u = 65535;
    v = 65535;
    sub0(u, v, &r);
    printf("sub0: %d-%d=%d\n", u, v, r);

    u = 65535;
    v = 100;
    sub0(u, v, &r);
    printf("sub0: %d-%d=%d\n", u, v, r);

    // SUB
    u = 65535;
    v = 65535;
    sub(u, v, &y[1], &y[0]);
    printf("sub: %d-%d=(%d,%d)\n", u, v, y[1], y[0]);

    u = 65535;
    v = 100;
    sub(u, v, &y[1], &y[0]);
    printf("sub: %d-%d=(%d,%d)\n", u, v, y[1], y[0]);

    // SUBb
    u = 65535;
    v = 65535;
    addc(u, v, 1, &y[1], &y[0]);
    printf("subb: %d-%d-1=(%d,%d)\n", u, v, y[1], y[0]);

    u = 65535;
    v = 100;
    subb(u, v, 1, &y[1], &y[0]);
    printf("subb: %d-%d-1=(%d,%d)\n", u, v, y[1], y[0]);

    // MUL0
    u = 65535;
    v = 65535;
    mul0(u, v, &r);
    printf("mul0: %d*%d=%d\n", u, v, r);

    u = 1000;
    v = 100;
    mul0(u, v, &r);
    printf("mul0: %d*%d=%d\n", u, v, r);

    u = 65535;
    v = 65535;
    mul(u, v, &y[1],&y[0]);
    printf("mul: %d*%d=(%d,%d)\n", u, v, y[1],y[0]);

    u = 1000;
    v = 100;
    mul(u, v, &y[1],&y[0]);
    printf("mul: %d*%d=(%d,%d)\n", u, v, y[1],y[0]);    

    u = 65535;
    v = 65535;
    a = 65535;
    b = 65535;        
    mulab(u, v, a, b, &y[1], &y[0]);
    printf("mulab: (%d*%d+%d+%d)=(%d,%d)\n", u, v, a, b, y[1], y[0]);
    
    u = 7;
    v = 11;
    a = 3;
    b = 20;
    mulab(u, v, a, b, &y[1], &y[0]);
    printf("mulab: %d*%d+%d+%d = (%d,%d)\n", u, v, a, b, y[1], y[0]); 

    exit(1);
}

#endif

