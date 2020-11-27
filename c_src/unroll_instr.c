// inline instructions for unroll expansion

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define TEST

#ifdef TEST
#define UINT_T  uint16_t
// #define UINTD_T uint32_t
#define UINTH_T uint8_t
#else
#include "unroll_auto_config.h"
#endif

#include "unroll.i"

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

