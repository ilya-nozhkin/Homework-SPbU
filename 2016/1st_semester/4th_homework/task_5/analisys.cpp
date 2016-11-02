#include "analisys.h"

int getOperatorPriority(char character)
{
    if (character == '+')
        return 1;
    else if (character == '-')
        return 1;
    else if (character == '*')
        return 2;
    else if (character == '/')
        return 2;
    return -1;
}

bool isItNumber(const char *expression, int position)
{
    if (position < 0)
        return false;

    char current = expression[position];
    char next = expression[position + 1];

    if (current == '-' && next >= '0' && next <= '9')
        return true;

    if (current >= '0' && current <= '9')
        return true;

    return false;
}

bool isItOperator(const char *expression, int position)
{
    if (position < 0)
        return false;

    char current = expression[position];
    return !isItNumber(expression, position) &&
            current != '(' && current != ')' && current != ' ';
}
