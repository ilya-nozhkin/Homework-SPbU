#include "Tokenizer.h"
#include "StateMachine.h"

#include <fstream>
#include <cstring>

using namespace std;

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
