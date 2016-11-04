#include <iostream>

using namespace std;

void inputData(int &a, int &n)
{
    cout << "Enter a and n (separated by space): ";
    cin >> a >> n;
}

void outputResult(uint64_t powerAN)
{
    cout << "a^n=" << powerAN;
}

uint64_t powerLogN(int a, int n)
{
    uint64_t auxiliary = a;
    uint64_t result = 1;
    while (n > 0)
    {
        if ((n & 1) == 1)
            result *= auxiliary;
            
        auxiliary *= auxiliary;
        
        n >>= 1;
    }
    return result;
}

uint64_t powerIterative(int a, int n)
{
    uint64_t result = n == 0 ? 1 : a;
    for (int i = 1; i < n; i++)
        result *= a;
        
    return result;
}

bool randomTest()
{
    const int maxA = 20;
    const int maxN = 14;
    
    int a = rand() % maxA + 1;
    int n = rand() % maxN + 1;
    
    uint64_t fastResult = powerLogN(a, n);
    uint64_t trueResult = powerIterative(a, n);
    
    if (fastResult != trueResult)
    {
        cout << "random test has been failed, a=" << a << " n=" << n << endl;
        cout << "got " << fastResult << " but expected " << trueResult << endl;
    }
    
    return fastResult == trueResult;
}

bool doRandomTests(int quantity)
{
    for (int i = 0; i < quantity; i++)
    {
        if (!randomTest())
            return false;
    }
    return true;
}

int main()
{
    srand(time(nullptr));
    
    const int randomTests = 100000;
    if (doRandomTests(randomTests))
    {
        cout << randomTests << " tests have been performed successfully, you can perform your own test:" << endl;
    }
    
    int a = 0;
    int n = 0;
    inputData(a, n);
    outputResult(powerLogN(a, n));
    
    return 0;
}
