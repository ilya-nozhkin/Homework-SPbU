#include "translator.h"
#include "stack.h"

#include <cstring>
#include <stdio.h>

void translateNumber(const char *expression, int &position, char *result, int &cursor)
{
    int number = 0;
    int offset = 0;

    sscanf(expression + position, "%d%n", &number, &offset);
    position += offset;

    sprintf(result + cursor, "%d %n", number, &offset);
    cursor += offset;
}

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

void addCharacter(char *string, char character)
{
    int length = strlen(string);
    string[length] = character;
    string[length + 1] = '\0';
}

void translateOperator(Stack *stack, const char *expression, int position, char *result, int &cursor)
{
    const int operatorsCount = 4;
    char current = expression[position];

    for (int i = 0; i < operatorsCount; i++)
    {
        int priority = getOperatorPriority(current);
        if (priority >= 0)
        {
            bool process = true;
            do
            {
                char last = top(stack);
                if (last >= 0 && last != '(' &&
                    priority <= getOperatorPriority(last))
                {
                    addCharacter(result, last);
                    addCharacter(result, ' ');
                    cursor += 2;
                    pop(stack);
                }
                else
                {
                    process = false;
                }
            } while (process);

            push(stack, current);
            break;
        }
    }
}

bool translateParenthesis(Stack *stack, char *result, int &cursor)
{
    bool process = true;
    do
    {
        if (!isEmpty(stack))
        {
            char last = top(stack);
            if (last == '(')
            {
                process = false;
            }
            else
            {
                addCharacter(result, last);
                addCharacter(result, ' ');
                cursor += 2;
            }
            pop(stack);
        }
        else
        {
            return false;
        }
    } while (process);
    return true;
}

bool translateStack(Stack *stack, char *result, int &cursor)
{
    while (!isEmpty(stack))
    {
        if (top(stack) == '(')
            return false;

        addCharacter(result, top(stack));
        addCharacter(result, ' ');
        cursor += 2;
        pop(stack);
    }

    return true;
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

bool hasDoubledOperatorsOrNumbers(const char *expression)
{
    int length = strlen(expression);
    int last = -1;

    int i = 0;
    while (i < length)
    {
        char current = expression[i];
        if (current != ' ')
        {
            if (isItNumber(expression, i))
            {
                if (isItNumber(expression, last))
                    return true;

                last = i;
                while (isItNumber(expression, i))
                    i++;
            }
            else
            {
                if (isItOperator(expression, i))
                {
                    if (isItOperator(expression, last))
                        return true;

                    last = i;
                }
                i++;
            }
        }
        else
        {
            i++;
        }
    }

    return false;
}

bool translateInfixToPostfix(const char *expression, char *result)
{
    if (hasDoubledOperatorsOrNumbers(expression))
        return false;

    Stack *stack = createStack();

    strcpy(result, "");
    int cursor = 0;

    int length = strlen(expression);

    int i = 0;
    while (i < length)
    {
        if (isItNumber(expression, i))
        {
            translateNumber(expression, i, result, cursor);
        }
        else
        {
            char current = expression[i];
            if (current == '(')
            {
                push(stack, current);
            }
            else if (current == ')')
            {
                if(!translateParenthesis(stack, result, cursor))
                    return false;
            }
            else if (current != ' ')
            {
                translateOperator(stack, expression, i, result, cursor);
            }

            i++;
        }
    }

    translateStack(stack, result, cursor);

    deleteStack(stack);

    return true;
}
