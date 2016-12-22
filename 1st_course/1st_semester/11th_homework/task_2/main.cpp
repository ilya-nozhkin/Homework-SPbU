#include "ExpressionAnalyzer.h"

#include <fstream>
#include <iostream>
#include <cstring>

using namespace std;

void inputLine(char *buffer, int size, const char *what)
{
    cout << "Enter " << what << ": ";
    cin.getline(buffer, size);
}

int main()
{
    const int bufferSize = 1024;
    char buffer[bufferSize] = {'\0'};
    inputLine(buffer, bufferSize, "an expression");

    char errorString[bufferSize] = {'\0'};
    bool status = checkExpression(buffer, "tokenizerFSM.txt", errorString);

    if (status)
        cout << "It is correct expression" << endl;
    else
        cout << errorString << endl;

    return 0;
}
