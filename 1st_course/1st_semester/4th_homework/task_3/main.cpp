#include <iostream>
#include <cstring>

#include "translator.h"

using namespace std;

int main()
{
    const int bufferSize = 512;

    char infixExpression[bufferSize] = {'\0'};
    cout << "enter some expression in infix notation: " << endl;
    cin.getline(infixExpression, bufferSize);

    char postfixExpression[bufferSize] = {'\0'};
    bool status = translateInfixToPostfix(infixExpression, postfixExpression);

    if (status)
    {
        cout << "your expression in postfix notation: " << endl;
        cout << postfixExpression << endl;
    }
    else
    {
        cout << "your expression is invalid" << endl;
    }

    return 0;
}
