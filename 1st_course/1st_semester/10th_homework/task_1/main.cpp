#include "Huffman.h"

#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

void reallocate(char *&buffer, int dataSize, int newSize)
{
    char *newBuffer = new char[newSize];
    for (int i = 0; i < dataSize; i++)
        newBuffer[i] = buffer[i];
    delete[] buffer;
    buffer = newBuffer;
}

char *readFile(ifstream &file)
{
    const int initialBufferSize = 16;
    const int inflateFactor = 2;

    int bufferSize = initialBufferSize;
    char *buffer = new char[bufferSize];

    int cursor = 0;
    while (!file.eof())
    {
        char symbol = 0;
        file >> symbol;
        buffer[cursor] = symbol;
        cursor++;

        if (cursor == bufferSize - 1)
        {
            bufferSize *= inflateFactor;
            reallocate(buffer, cursor, bufferSize);
        }
    }

    buffer[cursor] = '\0';

    return buffer;
}

bool checkFile(ifstream &file)
{
    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return false;
    }
    return true;
}

bool checkFile(ofstream &file)
{
    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return false;
    }
    return true;
}

char *readData(const char *fileName)
{
    ifstream sourceFile(fileName);
    if (!checkFile(sourceFile))
        return nullptr;

    char *source = readFile(sourceFile);

    sourceFile.close();

    return source;
}

bool writeResults(const char *fileName, HuffmanResult *result)
{
    ofstream destinationFile(fileName);

    if (!checkFile(destinationFile))
        return false;

    serializeToText(destinationFile, result);

    destinationFile.close();

    return true;
}

void outputStatistics(const char *data, HuffmanResult *result)
{
    uint64_t before = ((uint64_t) 8) * strlen(data);
    uint64_t after = result->compressedSize;

    cout << "original size: " << before << " bits" << endl;
    cout << "compressed size: " << after << " bits" << endl;
    cout << "compress factor: " << ((float) before / after) << endl;
}

int main()
{
    const int fileNameSize = 256;

    char sourceName[fileNameSize] = {'\0'};
    inputString(sourceName, "source file name");

    char *source = readData(sourceName);
    if (source == nullptr)
        return 1;

    HuffmanResult *result = compress(source);

    outputStatistics(source, result);

    char destinationName[fileNameSize] = {'\0'};
    inputString(destinationName, "where to save results");

    bool status = writeResults(destinationName, result);

    deleteResult(result);
    delete[] source;

    if (!status)
        return 2;

    return 0;
}
