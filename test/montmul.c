typedef unsigned short uint16_t;
typedef unsigned int   uint32_t;
typedef unsigned long  uint16_t;

#define UINT_T  uint32_t
#define UINTH_T uint16_t
#define UINTD_T uint64_t

#include "../c_src/unroll.i"

#define D_EXP (sizeof(UINT_T)*8)
#define D_MASK ((UINT_T)(-1))

#define ARR_SIZE(arr)  (sizeof((arr))/sizeof((arr)[0]))

typedef enum {
    REDC_DEFAULT,
    REDC_SOS,
    REDC_SPS
} redc_type_t;

static void big_copy(UINT_T* dst, UINT_T* src, int n)
{
    int i;
    for (i = 0; i < n; i++)
	dst[i] = src[i];
}

static void big_zero(UINT_T* dst, int n)
{
    int i;
    for (i = 0; i < n; i++)
	dst[i] = 0;
}

static int big_bits(UINT_T* x, int xl)
{
    int n = 8*sizeof(UINT_T)*(xl-1);
    UINT_T h = x[xl-1];
    if ((n == 0) && (h == 0))
	return 1;
    while(h) {
	n++;
	h >>= 1;
    }
    return n;
}

static int big_bit_test(UINT_T* x, int xl, unsigned pos)
{
    int d = pos / D_EXP; // digit
    pos %= D_EXP;      // bit
    if (d >= xl) return 0; // definied as zero
    return (x[d] & (1 << pos)) != 0;
}


static int big_comp(UINT_T* x, int xl, UINT_T* y, int yl)
{
    if (xl < yl)
	return -1;
    else if (xl > yl)
	return 1;
    else {
	if (x == y)
	    return 0;
	x += (xl-1);
	y += (yl-1);
	while((xl > 0) && (*x == *y)) {
	    x--;
	    y--;
	    xl--;
	}
	if (xl == 0)
	    return 0;
	return (*x < *y) ? -1 : 1;
    }
}

static int big_gt(UINT_T* x, int xl, UINT_T* y, int yl)
{
    return big_comp(x, xl, y, yl) > 0;
}

static int big_add(UINT_T* x, int xl, UINT_T* y, int yl,
		   UINT_T* r, int szr)
{
    UINT_T c = 0;
    int i = 0;

    while((i < xl) && (i < yl)) {
	addc(x[i],y[i],c,&c,&r[i]);
	i++;
    }
    while(i < xl) {
	addc(x[i],0,c,&c,&r[i]);
	i++;
    }
    while(i < yl) {
	addc(0,y[i],c,&c,&r[i]);	
	i++;
    }
    if (c)
	r[i++] = c;
    return i;
}

// x >= y
static int big_sub(UINT_T* x, int xl, UINT_T* y, int yl,
		   UINT_T* r, int szr)
{
    UINT_T b = 0;
    int i = 0;

    while(i < yl) {
	subb(x[i],y[i],b,&b,&r[i]);
	i++;
    }
    while(i < xl) {
	subb(x[i],0,b,&b,&r[i]);
	i++;
    }
    do {
	i--;
    } while((i>0) && (r[i] == 0));
    return i+1;
}

// inline shift x:xl k digits to the right
static int big_shr(UINT_T* x, int xl, int k)
{
    int n = xl - k;
    int i;
    for (i = 0; i < n; i++)
	x[i] = x[i+k];
    return n;
}

// multiply x and y module (B^k) (B is 2^w) (w = machine word size)
static int big_mul_k(UINT_T* x, int xl,
		     UINT_T* y, int yl,
		     UINT_T* r, int k)
{
    int i;

    for (i = 0; i < xl; i++) {
	UINT_T cp = 0;
	UINT_T c = 0;
	int j, ij;
	for (j = 0, ij=i; (j < yl) && (ij < k); j++, ij++) {
	    UINT_T p;
	    mula(x[i],y[j],cp,&cp,&p);
	    addc(p,r[ij],c,&c,&r[ij]);
	}
	if (ij < k)
	    r[ij] = c + cp;
    }
    i = ((xl+yl-1) < k) ? (xl+yl-1) : k-1;
    if (r[i] == 0)
	return i;
    else
	return i+1;
}


static int big_mul(UINT_T* x, int xl,
		   UINT_T* y, int yl,
		   UINT_T* r, int szr)
{
    int i;

    for (i = 0; i < xl; i++) {
	UINT_T cp = 0;
	UINT_T c = 0;
	int j, ij;
	for (j = 0, ij=i; (j < yl); j++, ij++) {
	    UINT_T p;
	    mula(x[i],y[j],cp,&cp,&p);
	    addc(p,r[ij],c,&c,&r[ij]);
	}
	r[ij] = c + cp;
    }
    i = xl+yl-1;
    if (r[i] == 0)
	return i;
    else
	return i+1;
}

static int big_sqr(UINT_T* x, int xl,
		   UINT_T* r, int szr)
{
    UINT_T d;
    int i, n;
    int ri, si;

    ri = si = i = 0;
    n = xl;
    while(n--) {
	UINT_T y_0 = 0, y_1 = 0, y_2 = 0, y_3 = 0;
	UINT_T b0, b1;
	UINT_T z0, z1, z2;
	int m = n;
	int ij;

	si = ri;
	
	d = x[i++];
	sqr(d, &z1, &b0);
	// mul(d, d, &z1, &b0);
	addc(r[si],b0,y_3,&y_3,&r[si]);
	si++;

	ij = i;
	while(m--) {
	    mul(d, x[ij], &b1, &b0);
	    ij++;
	    addc(b0, b0, y_0, &y_0, &z0);
	    addc(z0, z1, y_2, &y_2, &z2);
	    addc(r[si],z2,y_3,&y_3,&r[si]);
	    si++;
	    addc(b1, b1, y_1, &y_1, &z1);
	}
	z0 = y_0;
	addc(z0, z1, y_2, &y_2, &z2);
	addc(r[si], z2, y_3, &y_3, &r[si]);
	if (n != 0) {
	    si++;
	    r[si] = (y_1+y_2+y_3);
	    ri += 2;
	}
    }
    if (r[si] == 0)
	return si;
    else
	return si + 1;    
}

// note that P is destructivly updated
static int big_mont_redc_sos(UINT_T* P, int pl,
			     UINT_T* n, int s,
			     UINT_T* np, int npl,
			     UINT_T* Z, int szZ)
{
    int i, j;
    int Zl;
    UINT_T t = 0;
    
    for (i = 0; i < s; i++) {
	UINT_T u = 0;
	UINT_T v;
	UINT_T q;

	mul0(P[i],np[0],&q);

	for (j = 0; j < s; j++) {
	    mulab(n[j],q,P[i+j],u,&u,&v);
	    P[i+j] = v;
	}
	addc(P[i+s],t,u,&u,&P[i+s]);
	t = u;
    }
    for (j = 0; j < s; j++)
	Z[j] = P[j+s];
    Z[s] = t;
    i = s;
    while(i && (Z[i]==0)) i--;
    Zl = i+1;
    if (big_gt(Z, Zl, n, s)) // R>N?
	return big_sub(Z, Zl, n, s, Z, Zl);   // R = R - N
    return Zl;
}

static int big_mont_redc_sps(UINT_T* P, int pl,
			     UINT_T* n, int s,
			     UINT_T* np, int npl,
			     UINT_T* Z, int szZ)
{
    UINT_T t=0, u=0, v=0;
    UINT_T p0,p1;
    int i, j, Zl;
    
    for (i = 0; i < s; i++) {
	for (j = 0; j < i-1; j++) {
	    mul(Z[j],n[i-j],&p0,&p1);
	    add32(t,u,v,p1,p0,&t,&u,&v);
	}
	add31(t,u,v,P[i],&t,&u,&v);
	mul0(v, np[0], &Z[i]);
	mul(Z[i],n[0],&p0,&p1);
	add32(t,u,v,p1,p0,&t,&u,&v);
	v=u; u=t; t=0;
    }
    for (i = s; i < 2*(s-1); i++) {
	for (j = i-s+1; j < s-1; j++) {
	    mul(Z[j],n[i-j],&p0,&p1);
	    add32(t,u,v,p1,p0,&t,&u,&v);
	}
	add31(t,u,v,P[i],&t,&u,&v);
	Z[i-s] = v;
	v=u; u=t; t=0;
    }
    add31(t,u,v,P[2*s-1],&t,&u,&v);
    Z[s-1] = v;
    Z[s] = u;
    i = s;
    while(i && (Z[i]==0)) i--;
    Zl = i+1;    
    if (big_gt(Z, Zl, n, s))
	return big_sub(Z, Zl, n, s, Z, Zl);
    return Zl;    
}

static int big_mont_redc(redc_type_t redc_type,
			 UINT_T* t, int tl, UINT_T* n, int nl,
			 UINT_T* np, int npl, UINT_T* r, int szr)
{
    switch(redc_type) {
    case REDC_DEFAULT:
    case REDC_SOS:
	return big_mont_redc_sos(t, tl, n, nl, np, npl, r, szr);
    case REDC_SPS:
	return big_mont_redc_sps(t, tl, n, nl, np, npl, r, szr);
    default:
	return -1;
    }
}

// al,bl < nl < k   R[al+bl]
static int big_mont_mul(redc_type_t redc_type,
			UINT_T* a, int al, UINT_T* b, int bl,
			UINT_T* n, int nl,
			UINT_T* np, int npl,
			UINT_T* r, int szr)
{
    UINT_T T[2*nl];
    big_zero(T, 2*nl);
    big_mul(a, al, b, bl, T, ARR_SIZE(T));
    return big_mont_redc(redc_type, T, 2*nl, n, nl, np, npl, r, szr);
}

// al < nl < k
static int big_mont_sqr(redc_type_t redc_type,
			UINT_T* a, int al,
			UINT_T* n, int nl,
			UINT_T* np, int npl,
			UINT_T* r, int szr)
{
    UINT_T T[2*nl];
    big_zero(T, 2*nl);
    big_sqr(a, al, T, ARR_SIZE(T));
    return big_mont_redc(redc_type, T, 2*nl, n, nl, np, npl, r, szr);
}

// a^e (mod R)  (R=B^k > N)  (B = UINT_T base) r[2*al]
static int big_mont_pow(redc_type_t redc_type,
			UINT_T* a, int al,
			UINT_T* e, int el,
			UINT_T* p, int pl,
			UINT_T* n, int nl,
			UINT_T* np, int npl,
			UINT_T* R, int szR)
{
    UINT_T P[2][2*nl+1];
    UINT_T A[2][2*nl+1];
    int u, v;
    int s, t;
    int pos, nbits;
    int rl;

    u = 0; v = u^1;
    big_copy(P[u], p, pl);   // mont(1) !

    s = 0; t = s^1;
    big_copy(A[s], a, al);  // check al!

    nbits = big_bits(e, el)-1;
    for (pos = 0; pos < nbits; pos++) {
	int bit = big_bit_test(e, el, pos);
	if (bit) {
	    pl = big_mont_mul(redc_type,
			      A[s],al, P[u],pl, n,nl, np,npl,
			      P[v],ARR_SIZE(P[v]));
	    u = v; v = u^1;
	}
	al = big_mont_sqr(redc_type, A[s], al,
			  n, nl, np, npl, A[t], ARR_SIZE(A[t]));
	s = t; t = s^1;
    }
    rl = big_mont_mul(redc_type, A[s], al, P[u], pl, n, nl, np, npl, R, szR);
    return rl;
}

// al = nl = 32 digits
#define N_SIZE (1024 / sizeof(UINT_T))
int big_mont_pow_1024(redc_type_t redc_type,
		      UINT_T* a, int al,
		      UINT_T* e, int el,
		      UINT_T* p, int pl,
		      UINT_T* n, int nl,
		      UINT_T* np, int npl,
		      UINT_T* R, int szR)
{
    return big_mont_pow(REDC_SOS,
			a, N_SIZE,
			e, el,
			p, N_SIZE,
			n, N_SIZE,
			np, 1,
			R, 2*N_SIZE);
}

