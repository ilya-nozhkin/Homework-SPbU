#pragma once

#include <inttypes.h>
#include <istream>

struct HuffmanNode
{
    bool leaf;
    unsigned char symbol;
    HuffmanNode *left;
    HuffmanNode *right;
};

struct HuffmanSource
{
    HuffmanNode *treeRoot;

    uint8_t *data;
    uint64_t size;
};

HuffmanSource *deserializeFromText(std::istream &stream);
void deleteSource(HuffmanSource *&source);
char *decompress(HuffmanSource *source);
