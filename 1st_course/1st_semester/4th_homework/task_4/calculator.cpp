#include "calculator.h"
#include "stack.h"

#include <stdio.h>
#include <cstring>
#include <limits.h>

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

int executeOperator(char what, int left, int right)
{
    if (what == '+')
        return left + right;
    if (what == '-')
        return left - right;
    if (what == '*')
        return left * right;
    if (what == '/')
        return left / right;
    return INT_MIN;
}

int calculatePostfixExpression(const char *expression)
{
    Stack *stack = createStack();

    int cursor = 0;
    int length = strlen(expression);

    while (cursor < length)
    {
        if (isItNumber(expression, cursor))
        {
            int value = 0;
            int offset = 0;
            sscanf(expression + cursor, "%d%n", &value, &offset);
            push(stack, value);
            cursor += offset;
        }
        else
        {
            if (expression[cursor] != ' ')
            {
                if (isEmpty(stack))
                    return INT_MIN;
                int right = top(stack);
                pop(stack);

                if (isEmpty(stack))
                    return INT_MIN;
                int left = top(stack);
                pop(stack);

                int result = executeOperator(expression[cursor], left, right);
                if (result == INT_MIN)
                    return result;

                push(stack, result);
            }
            cursor++;
        }
    }

    if (isEmpty(stack))
        return INT_MIN;

    int answer = top(stack);

    deleteStack(stack);

    return answer;
}
