
void test()
{
    int x[10];
    int n;
    int k;

    n = sizeof(x)/sizeof(x[0]);
    k = sizeof(x)/sizeof(int);
    // fixme add typeof to bic
    // n = sizeof(x)/sizeof(typeof(x[0]));
}

