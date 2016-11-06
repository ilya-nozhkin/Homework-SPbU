#pragma once

struct String;

String *createString();
String *createString(const char *data);

void deleteString(String *&string);

String *clone(String *string);
String *substring(String *string, int start, int size);

void    concatenate(String *destination, String *source);

bool    equals(String *left, String *right);
int     length(String *string);
bool    isEmpty(String *string);

//I think this function is very dangerous because user gets an opportunity to access raw data inside structure
//If I could make my own signature I would choose this:
//      const char *castToChars(String *string);
char *castToChars(String *string);
