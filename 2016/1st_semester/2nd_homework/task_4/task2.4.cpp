#include <iostream>

using namespace std;

int computeGCD(int a, int b)
{
    while (a > 0 && b > 0)
    {
        if (a > b)
            a = a % b;
        else
            b = b % a;
    }
    return a + b;
}

int inputNumber(const char *what)
{
    int number = 0;
    cout << "Enter " << what << ": ";
    cin >> number;
    
    return number;
}

int compareFractions(int numerLeft, int denomLeft, int numerRight, int denomRight)
{
    int left = numerLeft * denomRight;
    int right = numerRight * denomLeft;
    return left < right ? -1 :
          (left > right ?  1 : 0);
}

void printFractions(int maxDenom)
{
    cout << "All possible fractions from (0; 1) with denominator <= " << maxDenom << endl;
    int lastNumer = 0;
    int lastDenom = maxDenom;
    while (lastNumer < lastDenom)
    {
        int numer = maxDenom;
        int denom = maxDenom;
        
        for (int i = maxDenom; i > 0; i--)
            for (int j = 1; j < i; j++)
                if (computeGCD(j, i) == 1)
                {
                    if (compareFractions(j, i, numer,     denom)     < 0 && 
                        compareFractions(j, i, lastNumer, lastDenom) > 0)
                    {
                        numer = j;
                        denom = i;
                    }
                }
                
        lastNumer = numer;
        lastDenom = denom;
        
        if (numer < maxDenom)
            cout << numer << " / " << denom << endl;
    }
}

int main()
{
    int maxDenominator = inputNumber("the maximum denominator");
    printFractions(maxDenominator);
    
    return 0;
}
