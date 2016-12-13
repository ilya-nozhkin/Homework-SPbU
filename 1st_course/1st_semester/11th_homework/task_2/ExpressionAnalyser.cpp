#include "ExpressionAnalyzer.h"
#include "StateMachine.h"

#include <cstring>
#include <fstream>
#include <stdio.h>

using namespace std;

struct Token
{
    char type;
    int position;
};

enum TokenizerState
{
    firstNumberState = 1,
    secondNumberState = 3,
    thirdNumberState = 6,
    plusState = 7,
    minusState = 8,
    multiplyState = 9,
    divisionState = 10,
    errorState = 11
};

void fillToken(Token &token, int state, int position)
{
    switch (state)
    {
        case firstNumberState:
        case secondNumberState:
        case thirdNumberState:
        {
            token.type = 'N';
            break;
        }
        case plusState:
        {
            token.type = '+';
            break;
        }
        case minusState:
        {
            token.type = '-';
            break;
        }
        case multiplyState:
        {
            token.type = '*';
            break;
        }
        case divisionState:
        {
            token.type = '/';
            break;
        }
    }

    token.position = position;
}

int tokenize(const char *expression, Token *tokens, const char *tokenizerFSM, int &errorPosition)
{
    StateMachine *tokenizer = createStateMachine();

    ifstream table(tokenizerFSM);
    deserialize(tokenizer, table);
    table.close();

    int length = strlen(expression);

    int tokensNumber = 0;

    setState(tokenizer, 0);
    int cursor = 0;
    int start = 0;
    bool error = false;
    while (cursor < length + 1 && !error)
    {
        int previous = getState(tokenizer);
        step(tokenizer, cursor == length ? ' ' : expression[cursor]);
        int current = getState(tokenizer);

        if (current == errorState)
            error = true;
        else
        {
            if (current == 0 && current != previous)
            {
                fillToken(tokens[tokensNumber], previous, start);
                start = cursor;
                tokensNumber++;
            }
            else
            {
                cursor++;
                if (current == previous)
                    start = cursor;
            }
        }
    }

    deleteStateMachine(tokenizer);

    if (error)
    {
        errorPosition = cursor;
        return 0;
    }

    return tokensNumber;
}

bool number(Token *tokens, int tokensNumber, int &cursor)
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

bool operations(Token *tokens, int tokensNumber, int &cursor);

bool operation(Token *tokens, int tokensNumber, int &cursor, char type)
{
    if (cursor == tokensNumber) //epsilon
        return true;

    if (tokens[cursor].type == type)
    {
        cursor++;
        if (number(tokens, tokensNumber, cursor))
            return operations(tokens, tokensNumber, cursor);
        else
            return false;
    }

    return false;
}

bool operations(Token *tokens, int tokensNumber, int &cursor)
{
    int stored = cursor;

    if (operation(tokens, tokensNumber, cursor, '+'))
        return true;
    cursor = stored;

    if (operation(tokens, tokensNumber, cursor, '-'))
        return true;
    cursor = stored;

    if (operation(tokens, tokensNumber, cursor, '*'))
        return true;
    cursor = stored;

    if (operation(tokens, tokensNumber, cursor, '/'))
        return true;

    return false;
}

bool base(Token *tokens, int tokensNumber, int &cursor)
{
    if (cursor >= tokensNumber)
        return false;

    if (number(tokens, tokensNumber, cursor))
        return operations(tokens, tokensNumber, cursor);

    return false;
}

//grammar:
//base -> NO
//O -> +NO | -NO | *NO | /NO | e
//where: O - operation, N - number
bool checkExpression(const char *expression, const char *tokenizerFSM, char *errorString)
{
    Token *tokens = new Token[strlen(expression)];

    int errorPosition = 0;

    int tokensNumber = tokenize(expression, tokens, tokenizerFSM, errorPosition);

    bool status = false;
    if (tokensNumber > 0)
    {
        int cursor = 0;
        status = base(tokens, tokensNumber, cursor);
        sprintf(errorString, "Syntax error");
    }
    else
        sprintf(errorString, "Lexical error on position %d", errorPosition);

    delete[] tokens;
    return status;
}
