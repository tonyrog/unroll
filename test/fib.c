//
// Fibonacci sequence
//    

int fib(int n)
{
    int f0 = 1, f1 = 1;
    int i;

    for (i = 1; i <= n; i++) {
	int tmp = f1;

	f1 = f1 + f0;
	f0 = tmp;
    }
    return f1;
}

int fib_5()
{
    fib(5);
}

int rfib(int n)
{
    if (n == 0) return 0;
    if (n == 1) return 1;
    return rfib(n-1) + rfib(n-2);
}

int rfib_5()
{
    return rfib(5);
}
