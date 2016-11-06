#include "myString.h"

#include <cstring>

struct String
{
    char *buffer;
    int length;
};

String *createString()
{
    String *string = new String;
    string->buffer = new char[1];
    string->buffer[0] = '\0';
    string->length = 0;
    return string;
}

String *createString(const char *data)
{
    String *string = new String;

    int size = strlen(data);
    string->buffer = new char[size + 1];
    string->length = size;
    strcpy(string->buffer, data);

    return string;
}

void deleteString(String *&string)
{
    delete[] string->buffer;
    delete string;
    string = nullptr;
}

String *clone(String *string)
{
    return createString(castToChars(string));
}

String *substring(String *string, int start, int size)
{
    String *newString = new String;

    newString->buffer = new char[size + 1];
    newString->length = size;
    strncpy(newString->buffer, string->buffer + start, size);
    newString->buffer[size] = 0;

    return newString;
}

void concatenate(String *destination, String *source)
{
    int size = length(destination) + length(source);
    char *buffer = new char[size + 1];

    strcpy(buffer, destination->buffer);
    strcat(buffer, source->buffer);

    delete[] destination->buffer;
    destination->buffer = buffer;
    destination->length = size;
}

bool equals(String *left, String *right)
{
    return strcmp(left->buffer, right->buffer) == 0;
}

int length(String *string)
{
    return string->length;
}

bool isEmpty(String *string)
{
    return string->length == 0;
}

char *castToChars(String *string)
{
    return string->buffer;
}
