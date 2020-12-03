int foo(int a, int b)
{
    int x = a;
    while ((b < 5) && (a == 1)) {
	int a = 3;
	{
	    int a = a+b;
	    b += a;
	}
	b += a;
    }
    return a+b;
}

