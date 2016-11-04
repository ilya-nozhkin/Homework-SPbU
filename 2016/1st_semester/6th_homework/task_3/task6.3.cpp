#include <iostream>
#include <math.h>

using namespace std;

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

int lengthOfInt(int number)
{
    if (number < 0)
        number = -number;
    
    int length = 0;
    while (number > 0)
    {
        length++;
        number /= 10;
    }
    return length;
}

void printSpaces(int length)
{
    for (int i = 0; i < length; i++)
        cout << ' ';
}

void printFirstLine(int degree, int *coefficients)
{
    printSpaces(coefficients[0] < 0 ? 2 : 1);
    for (int i = degree; i >= 2; i--)
    {
        int current = coefficients[degree - i];
        int length = lengthOfInt(current);
        if (length > 0)
        {
            if (abs(current) > 1)
                printSpaces(length);
                
            cout << i;
            printSpaces(3);
        }
    }
    
    if (degree > 1)
        cout << endl;
}

void printSecondLine(int degree, int *coefficients)
{
    for (int i = degree; i >= 0; i--)
    {
        int current = coefficients[degree - i];
        
        if (current != 0)
        {
            if (current < 0)
            {
                cout << '-';
                current = -current;
            }
            else if (i < degree)
                    cout << '+';
            
            if (i < degree)
                cout << ' ';
            
            if (abs(current) > 1 || i == 0)    
                cout << current;
            
            if (i > 0) 
                cout << 'x';
                
            printSpaces(lengthOfInt(i));
        }
    }
    
    cout << endl;
}

void printPolynomialBeautifully(int degree, int *coefficients)
{
    printFirstLine(degree, coefficients);
    printSecondLine(degree, coefficients);
}

int main()
{
    int degree = inputNumber("the degree of polynomial");
    
    int *coefficients = new int[degree + 1];
    
    cout << "Enter coefficients: ";
    for (int i = 0; i < degree + 1; i++)
        cin >> coefficients[i];
    
    cout << "Polynomial with these coefficients:" << endl << endl;
    printPolynomialBeautifully(degree, coefficients);
    
    delete[] coefficients;
    
    return 0;
}
