#pragma once

struct Token
{
    char type;
    int position;
};

int tokenize(const char *expression, Token *tokens, const char *tokenizerFSM, int &errorPosition);
