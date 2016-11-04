#include <iostream>

using namespace std;

void printIterativeFibonacci(int penultimate, int last, int number)
{
    for (int i = 0; i < number; i++)
    {
        int current = penultimate + last;
        cout << current << ' ';
        
        penultimate = last;
        last = current;
    }
}

void printRecursiveFibonacci(int penultimate, int last, int number)
{
    if (number == 0) 
    {
        return;
    }
    
    int current = penultimate + last;
    cout << current << ' ';
    
    printRecursiveFibonacci(last, current, number - 1);
}

inline void printFirstNumbers(int *source, int sourceSize, int quantity)
{
    int size = (quantity <= sourceSize ? quantity : sourceSize);
    for (int i = 0; i < size; i++)
        cout << source[i] << ' ';
}

int main()
{
    int quantity = 0;
    cout << "Enter the number of fibonacci numbers: ";
    cin >> quantity;
    
    int sourceSize = 2;
    int source[sourceSize] = {0, 1};
    
    cout << "Fibonacci sequence produced by iterative function: " << endl;
    printFirstNumbers(source, sourceSize, quantity);
    if (quantity > 2) 
    {
        printIterativeFibonacci(source[0], source[1], quantity - 2);
    }
    cout << endl;
    
    cout << "Fibonacci sequence produced by recursive function: " << endl;
    printFirstNumbers(source, sourceSize, quantity);
    if (quantity > 2) 
    {
        printRecursiveFibonacci(source[0], source[1], quantity - 2);
    }
    cout << endl;
    
    return 0;
}
