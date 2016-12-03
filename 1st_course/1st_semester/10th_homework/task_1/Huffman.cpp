#include "Huffman.h"

#include <cstring>

HuffmanResult *initializeResult()
{
    HuffmanResult *result = new HuffmanResult;
    for (int i = 0; i < maxSymbols; i++)
    {
        result->codes[i] = 0;
        result->codeLength[i] = 0;
    }

    result->compressed = nullptr;
    result->compressedSize = 0;

    return result;
}

void deleteNode(HuffmanNode *&node)
{
    if (node == nullptr)
        return;

    deleteNode(node->left);
    deleteNode(node->right);

    delete node;
    node = nullptr;
}

void deleteResult(HuffmanResult *&result)
{
    delete[] result->compressed;
    deleteNode(result->treeRoot);

    delete result;
    result = nullptr;
}

HuffmanNode *moveNode(HuffmanNode &node)
{
    HuffmanNode *copy = new HuffmanNode;
    *copy = node;

    node.leaf = false;
    node.symbol = 0;
    node.count = 0;
    node.left = nullptr;
    node.right = nullptr;

    return copy;
}

void initializeLeaf(HuffmanNode &node, unsigned char symbol)
{
    node.leaf = true;
    node.symbol = symbol;
    node.count = 1;
    node.left = nullptr;
    node.right = nullptr;
}

void countSymbols(const char *data, HuffmanNode *leafs, int &symbols)
{
    int length = strlen(data);

    int references[maxSymbols];
    for (int i = 0; i < maxSymbols; i++)
        references[i] = -1;

    for (int i = 0; i < length; i++)
    {
        unsigned char symbol = (unsigned char) data[i];
        if (references[symbol] == -1)
        {
            initializeLeaf(leafs[symbols], symbol);
            references[symbol] = symbols;
            symbols++;
        }
        else
        {
            leafs[references[symbol]].count++;
        }
    }
}

void swap(HuffmanNode &left, HuffmanNode &right)
{
    HuffmanNode temp = left;
    left = right;
    right = temp;
}

void insertionSort(HuffmanNode *nodes, int count, int sortedPrefix)
{
    for (int i = sortedPrefix; i < count; i++)
    {
        int j = i;
        while (j > 0 && nodes[j].count > nodes[j - 1].count)
        {
            swap(nodes[j], nodes[j - 1]);
            j--;
        }
    }
}

void makeTree(HuffmanNode *nodes, int symbols)
{
    insertionSort(nodes, symbols, 1);

    while (symbols > 1)
    {
        HuffmanNode *left = moveNode(nodes[symbols - 2]);
        HuffmanNode *right = moveNode(nodes[symbols - 1]);

        symbols--;
        HuffmanNode &last = nodes[symbols - 1];
        last.leaf = false;
        last.count = left->count + right->count;
        last.left = left;
        last.right = right;

        insertionSort(nodes, symbols, symbols - 1);
    }
}

void extractCodes(HuffmanNode *node, HuffmanResult *result, uint8_t code, int length)
{
    if (node->leaf)
    {
        unsigned char symbol = node->symbol;

        if (length == 0)
        {
            result->codeLength[symbol] = 1;
            result->codes[symbol] = 0;
        }
        else
        {
            result->codeLength[symbol] = length;
            result->codes[symbol] = code;
        }
    }
    else
    {
        extractCodes(node->left, result, (code << 1) + 0, length + 1);
        extractCodes(node->right, result, (code << 1) + 1, length + 1);
    }
}

void moveTree(HuffmanResult *result, HuffmanNode &root)
{
    result->treeRoot = moveNode(root);
}

inline uint8_t getBit(uint8_t byte, int bit)
{
    return (byte >> bit) & 1;
}

void convert(HuffmanResult *result, const char *data)
{
    int lengthSum = 0;
    int presented = 0;
    for (int i = 0; i < maxSymbols; i++)
        if (result->codeLength[i] > 0)
        {
            lengthSum += result->codeLength[i];
            presented++;
        }

    int average = (lengthSum + presented) / presented;

    int length = strlen(data);

    int bufferSize = length * average;
    result->compressed = new uint8_t[bufferSize];
    for (int i = 0; i < bufferSize; i++)
        result->compressed[i] = 0;

    uint64_t bitCursor = 0;
    uint32_t byteCursor = 0;

    for (int i = 0; i < length; i++)
    {
        unsigned char symbol = (unsigned char) data[i];
        uint8_t code = result->codes[symbol];
        uint8_t codeLength = result->codeLength[symbol];

        for (int i = codeLength - 1; i >= 0; i--)
        {
            uint8_t &current = result->compressed[byteCursor];
            uint8_t offset = 7 - bitCursor % 8;
            if (getBit(code, i))
                current |= 1 << offset;

            bitCursor++;
            if (bitCursor % 8 == 0)
                byteCursor++;
        }
    }

    result->compressedSize = bitCursor;
}

HuffmanResult *compress(const char *data)
{
    HuffmanResult *result = initializeResult();

    HuffmanNode nodes[maxSymbols];
    int symbols = 0;

    countSymbols(data, nodes, symbols);
    makeTree(nodes, symbols);
    extractCodes(&nodes[0], result, 0, 0);
    moveTree(result, nodes[0]);
    convert(result, data);

    return result;
}

void serializeNodeToText(std::ostream &stream, HuffmanNode *node)
{
    if (node->leaf)
        stream << (char) node->symbol;
    else
    {
        stream << '(';
        serializeNodeToText(stream, node->left);
        stream << ' ';
        serializeNodeToText(stream, node->right);
        stream << ')';
    }
}

void serializeToText(std::ostream &stream, HuffmanResult *result)
{
    serializeNodeToText(stream, result->treeRoot);
    stream << std::endl;

    for (uint64_t i = 0; i < result->compressedSize; i++)
    {
        uint32_t byte = result->compressed[i / 8];
        stream << (getBit(byte, 7 - (i % 8)) == 0 ? '0' : '1');
    }

    stream << std::endl;
}
