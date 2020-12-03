typedef unsigned UINT_T;
// #define UINT_T unsigned long

void addc(UINT_T src1, UINT_T src2, UINT_T ci, UINT_T* co, UINT_T* sum);
//void (*addc)(UINT_T src1, UINT_T src2, UINT_T ci, UINT_T* co, UINT_T* sum);

int big_add(UINT_T x[], int xl, UINT_T y[], int yl, UINT_T r[])
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

int big_add_nm(UINT_T x[], UINT_T y[], UINT_T r[])
{
    return big_add(x, 4, y, 4, r);
}
