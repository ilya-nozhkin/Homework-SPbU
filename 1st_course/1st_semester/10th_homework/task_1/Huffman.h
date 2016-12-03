#pragma once

#include <inttypes.h>
#include <ostream>

const int maxSymbols = 256;

struct HuffmanNode
{
    bool leaf;
    unsigned char symbol;
    int count;
    HuffmanNode *left;
    HuffmanNode *right;
};

struct HuffmanResult
{
    HuffmanNode *treeRoot;

    uint8_t codes[maxSymbols];
    uint8_t codeLength[maxSymbols];

    uint8_t *compressed;
    uint64_t compressedSize;
};

void deleteResult(HuffmanResult *&result);

HuffmanResult *compress(const char *data);

void serializeToText(std::ostream &stream, HuffmanResult *result);
