#include <iostream>

using namespace std;

bool printPossibleSums(int number, int residue, int lastSummand, char *buffer, int cursor)
{
    int begin = residue < lastSummand ? residue : lastSummand;
    for (int summand = begin; summand > 0; summand--)
    {
        if (residue - summand == 0)
        {
            cout << buffer << summand << '=' << number << endl;
        }
        else
        {
            int shift = 0;
            sprintf(buffer + cursor, "%d+%n", summand, &shift);
            printPossibleSums(number, residue - summand, summand, buffer, cursor + shift);
        }
    }
    return false;
}

int main()
{
    int number = 0;
    cout << "Enter the number: ";
    cin >> number;
    
    //buffer size is computed for the worst sum: 1+1+1+...+1=number - 
    //2 chars for each 1 and 16 chars for '=' and number
    int bufferSize = 2 * number + 16;
    char *buffer = new char[bufferSize];
    
    int cursor = 0;
    buffer[cursor] = 0;
    
    cout << "All possible sums which are equal to " << number << ":" << endl;
    printPossibleSums(number, number, number, buffer, cursor);
    
    delete[] buffer;
    
    return 0;
}
