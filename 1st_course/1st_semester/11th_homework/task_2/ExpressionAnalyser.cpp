#include "ExpressionAnalyzer.h"
#include "StateMachine.h"
#include "Tokenizer.h"

#include <cstring>
#include <fstream>
#include <stdio.h>

using namespace std;

bool grammarN(Token *tokens, int tokensNumber, int &cursor)
{
    if (cursor >= tokensNumber)
        return false;

    if (tokens[cursor].type == 'N')
    {
        cursor++;
        return true;
    }

    if (tokensNumber - cursor >= 2 &&
        (tokens[cursor].type == '-' ||
         tokens[cursor].type == '+') &&
        tokens[cursor + 1].type == 'N')
    {
        cursor += 2;
        return true;
    }

    return false;
}

bool grammarT(Token *tokens, int tokensNumber, int &cursor);

bool grammarTa(Token *tokens, int tokensNumber, int &cursor)
{
    bool status = false;
    int stored = cursor;

    if (cursor < tokensNumber)
    {
        if (tokens[cursor].type == '*')
        {
            cursor++;
            status = grammarT(tokens, tokensNumber, cursor);
            if (status)
                return true;
        }

        cursor = stored;
        if (tokens[cursor].type == '/')
        {
            cursor++;
            status = grammarT(tokens, tokensNumber, cursor);
            if (status)
                return status;
        }
    }

    cursor = stored;
    return true; //epsilon
}

bool grammarT(Token *tokens, int tokensNumber, int &cursor)
{
    if (grammarN(tokens, tokensNumber, cursor))
        return grammarTa(tokens, tokensNumber, cursor);

    return false;
}

bool grammarE(Token *tokens, int tokensNumber, int &cursor);

bool grammarEa(Token *tokens, int tokensNumber, int &cursor)
{
    bool status = false;
    int stored = cursor;

    if (cursor < tokensNumber)
    {
        if (tokens[cursor].type == '+')
        {
            cursor++;
            status = grammarE(tokens, tokensNumber, cursor);
            if (status)
                return true;
        }

        cursor = stored;
        if (tokens[cursor].type == '-')
        {
            cursor++;
            status = grammarE(tokens, tokensNumber, cursor);
            if (status)
                return status;
        }
    }

    cursor = stored;
    return true; //epsilon
}

bool grammarE(Token *tokens, int tokensNumber, int &cursor)
{
    if (grammarT(tokens, tokensNumber, cursor))
        return grammarEa(tokens, tokensNumber, cursor);

    return false;
}

bool grammarCheck(Token *tokens, int tokensNumber, int &errorToken)
{
    int cursor = 0;
    if (grammarE(tokens, tokensNumber, cursor))
    {
        if (cursor != tokensNumber)
        {
            errorToken = cursor;
            return false;
        }
        else
            return true;
    }
    return false;
}

//left-recursive grammar:
//E -> E + T | E - T | T
//T -> T * F | T / F | F

//normal grammar:
//E  -> TE'
//E' -> +TE' | -TE' | e
//T  -> NT'
//T' -> *NT' | /NT' | e

//optimized grammar:
//E  -> TE'
//E' -> +E | -E | e
//T  -> NT'
//T' -> *T | /T | e

void generateErrorMessage(char *buffer, const char *expression, const char *errorText, int errorPosition)
{
    int offset = 0;
    sprintf(buffer, "%s\n%s\n%n", errorText, expression, &offset);

    for (int i = 0; i < errorPosition; i++)
        buffer[offset + i] = ' ';

    buffer[offset + errorPosition] = '^';
    buffer[offset + errorPosition + 1] = '\n';
    buffer[offset + errorPosition + 2] = '\0';
}

bool checkExpression(const char *expression, const char *tokenizerFSM, char *errorString)
{
    Token *tokens = new Token[strlen(expression)];

    int errorPosition = 0;

    int tokensNumber = tokenize(expression, tokens, tokenizerFSM, errorPosition);

    bool status = false;
    if (tokensNumber > 0)
    {
        status = grammarCheck(tokens, tokensNumber, errorPosition);
        if (!status)
            generateErrorMessage(errorString, expression, "Syntax error (invalid token sequence)",
                                 tokens[errorPosition].position);
    }
    else
        generateErrorMessage(errorString, expression, "Lexical error (unexpected character)", errorPosition);

    delete[] tokens;
    return status;
}
