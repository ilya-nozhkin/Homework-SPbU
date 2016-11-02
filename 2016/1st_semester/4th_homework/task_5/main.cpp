#include "calculator.h"

#include <iostream>
#include <limits.h>

using namespace std;

int main()
{
    const int bufferSize = 512;

    char postfixExpression[bufferSize] = {'\0'};
    cout << "enter some expression in infix notation: " << endl;
    cin.getline(postfixExpression, bufferSize);

    int result = calculateInfixExpression(postfixExpression);
    if (result > INT_MIN)
    {
        cout << "result equals " << result << endl;
    }
    else
    {
        cout << "your expression is invalid" << endl;
    }

    return 0;
}
