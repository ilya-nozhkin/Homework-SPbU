#include "Huffman.h"

#include <cstring>

HuffmanNode *createNode()
{
    HuffmanNode *node = new HuffmanNode;
    node->leaf = false;
    node->symbol = '\0';
    node->left = nullptr;
    node->right = nullptr;
    return node;
}

void deleteNode(HuffmanNode *&node)
{
    if (node->left != nullptr)
        deleteNode(node->left);
    if (node->right != nullptr)
        deleteNode(node->right);
    delete node;
    node = nullptr;
}

void parseNode(HuffmanNode *&node, const char *buffer, int &cursor)
{
    char current = buffer[cursor];
    cursor++;

    node = createNode();

    bool leaf = current != '(';

    //if current node is leaf but it's symbol is '('
    if (current == '(' && buffer[cursor] == ' ' && buffer[cursor + 1] != ' ')
        leaf = true;

    node->leaf = leaf;

    if (leaf)
        node->symbol = (unsigned char) current;
    else
    {
        parseNode(node->left, buffer, cursor);
        cursor++; //space
        parseNode(node->right, buffer, cursor);
        cursor++;
    }
}

void parseTree(HuffmanSource *source, const char *buffer)
{
    int cursor = 0;
    parseNode(source->treeRoot, buffer, cursor);
}

HuffmanSource *createHuffmanSource()
{
    HuffmanSource *source = new HuffmanSource;
    source->data = nullptr;
    source->size = 0;
    source->treeRoot = nullptr;
    return source;
}

void deleteSource(HuffmanSource *&source)
{
    if (source->data != nullptr)
        delete[] source->data;
    if (source->treeRoot != nullptr)
        deleteNode(source->treeRoot);

    delete source;
    source = nullptr;
}

void reallocate(char *&buffer, int dataSize, int newSize)
{
    char *newBuffer = new char[newSize];
    for (int i = 0; i < dataSize; i++)
        newBuffer[i] = buffer[i];
    delete[] buffer;
    buffer = newBuffer;
}

char *readLine(std::istream &stream)
{
    const int initialBufferSize = 16;
    const int inflateFactor = 2;

    int bufferSize = initialBufferSize;
    char *buffer = new char[bufferSize];

    int cursor = 0;
    bool process = true;
    while (process)
    {
        char symbol = stream.get();

        if (symbol == '\n' || symbol == EOF)
            process = false;\
        else
        {
            buffer[cursor] = symbol;
            cursor++;

            if (cursor == bufferSize - 1)
            {
                bufferSize *= inflateFactor;
                reallocate(buffer, cursor, bufferSize);
            }
        }
    }

    buffer[cursor] = '\0';

    return buffer;
}

HuffmanSource *deserializeFromText(std::istream &stream)
{
    char *treeString = readLine(stream);
    char *dataString = readLine(stream);

    HuffmanSource *source = createHuffmanSource();

    parseTree(source, treeString);

    source->size = strlen(dataString);

    int bufferSize = (source->size + 8) / 8;
    source->data = new uint8_t[bufferSize];
    for (int i = 0; i < bufferSize; i++)
        source->data[i] = 0;

    for (uint64_t i = 0; i < source->size; i++)
    {
        int byte = i / 8;
        int bit = 7 - (i % 8);

        char value = dataString[i];
        if (value == '1')
            source->data[byte] |= 1 << bit;
    }

    delete[] treeString;
    delete[] dataString;

    return source;
}

inline uint8_t getBit(uint8_t byte, int bit)
{
    return (byte >> bit) & 1;
}

char *decompress(HuffmanSource *source)
{
    char *result = new char[source->size + 1];

    HuffmanNode *node = source->treeRoot;
    if (node->leaf)
    {
        for (uint64_t i = 0; i < source->size; i++)
            result[i] = (char) node->symbol;
        result[source->size] = '\0';
        return result;
    }

    int cursor = 0;
    for (uint64_t i = 0; i < source->size; i++)
    {
        int byte = i / 8;
        int bit = 7 - (i % 8);
        uint8_t value = getBit(source->data[byte], bit);

        node = value == 0 ? node->left : node->right;
        if (node->leaf)
        {
            result[cursor] = (char) node->symbol;
            cursor++;

            node = source->treeRoot;
        }
    }

    result[cursor] = '\0';
    reallocate(result, cursor + 1, cursor + 1);

    return result;
}
