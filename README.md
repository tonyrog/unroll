# unroll - Code unrolling and generation

Unroll is aimed at unrolling C code produced by
the bic C parser (https://github.com/tonyrog/bic)

# big digit support

Algorithms implementing bignum operations need some basic code
fragments that handle carry, borrow and overflow.
There are some included in "unroll.i" that with the
latest compilers yied more or less optimal code.

Add digits x and y and return the sum.

    void add0(UINT_T x, UINT_T y, UINT_T* s);
	
		s = x+y;
	
Add digits x and y and return the sum and a carry.

	void add(UINT_T x, UINT_T y, UINT_T* co, UINT_T* s);
	
		s = x+y;
		co = carry(x+y);
	
Add digits x and y and a ci, return the sum and a the resulting  carry.

	void addc(UINT_T x, UINT_T y, UINT_T ci, UINT_T* co, UINT_T* s);
	
		s = x+y + ci;
		co = carry(x+y);	

Subtract x and y, return difference.

	void sub0(UINT_T x, UINT_T y, UINT_T* d);
	
		d = x-y;
		
Subtract x and y, return difference and a borrow.

	void sub(UINT_T x, UINT_T y, UINT_T* bo, UINT_T* d);
	
		d = x-y;
		bo = borrow(x-y);

Subtract x and y with borrow, return difference and a resulting borrow.

	void subb(UINT_T x, UINT_T y, UINT_T bi, UINT_T* bo, UINT_T* d);
	
		d = x-(y+bi);
		bo = borrow(x-(y+bi));

Multiply x and y and return the single word result in p0

	void mul0(UINT_T x, UINT_T y, UINT_T* p0);
	
Multiply x and y and return the product in (p1,p0)	where p1 is the
high bits of the product and p0 is the low bits of the product
	
	void mul(UINT_T x, UINT_T y, UINT_T* p1, UINT_T* p0);
	
		(p1,p0) = x*y;

Add a to the product of x and y, return result as (p1,p0)

	void mula(UINT_T x, UINT_T y, UINT_T a, UINT_T* p1, UINT_T* p0);
	
		(p1,p0) = x*y + a;
		
Square x and return result as (p1,p0)

	void sqr(UINT_T x, UINT_T* p1, UINT_T* p0);
	
		(p1,p0) = x*x;

Square x add a and return result as (p1,p0)

	void sqra(UINT_T x, UINT_T a, UINT_T* p1, UINT_T* p0);	

		(p1,p0) = x*x + a;
	
Add both a and b to the product of x and y, return result as (p1,p0)

	void mulab(UINT_T x, UINT_T y, UINT_T a, UINT_T b, UINT_T* p1, UINT_T* p0);
	
		(p1,p0) = x*x + a + b;
		
