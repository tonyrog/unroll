typedef unsigned UINT_T;

void mula(UINT_T src1, UINT_T src2, UINT_T a, UINT_T* prod1, UINT_T* prod0);
void add0(UINT_T src1, UINT_T src2, UINT_T* sum);
void addc(UINT_T src1, UINT_T src2, UINT_T ci, UINT_T* co, UINT_T* sum);

int big_mul(UINT_T x[], int xl, UINT_T y[], int yl, UINT_T r[])
{
    UINT_T c, cp;
    UINT_T ij, p;
    int i;

    for (i = 0; i < xl; i++) {
	UINT_T c=0, cp=0;
	int ij = i;
	int j;
	for (j = 0; j < yl; j++) {
	    mula(x[i],y[j],cp,&cp,&p);
	    addc(p,r[ij],c,&c,&r[ij]);
	    ij++;
	}
	add0(c,cp,&r[ij]);
    }
    return (r[xl+yl-1]==0) ? xl+yl-1 : xl+yl;
}

int big_mul_4_4(UINT_T x[], UINT_T y[], UINT_T r[])
{
    return big_mul(x, 4, y, 4, r);
}

int big_mul_16_16(UINT_T x[], UINT_T y[], UINT_T r[])
{
    return big_mul(x, 16, y, 16, r);
}
