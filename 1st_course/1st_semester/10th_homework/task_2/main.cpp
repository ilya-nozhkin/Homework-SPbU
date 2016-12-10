#include "Huffman.h"

#include <iostream>
#include <fstream>

using namespace std;

HuffmanSource *readSource(const char *fileName)
{
    ifstream file(fileName);

    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return nullptr;
    }

    return deserializeFromText(file);
}

bool writeResult(const char *fileName, const char *result)
{
    ofstream file(fileName);

    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return false;
    }

    file << result;
    return true;
}

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

int main()
{
    const int fileNameSize = 256;

    char sourceName[fileNameSize] = {'\0'};
    inputString(sourceName, "source file name");

    HuffmanSource *source = readSource(sourceName);
    if (source == nullptr)
        return 1;

    char *result = decompress(source);

    cout << result << endl;

    char destinationName[fileNameSize];
    inputString(destinationName, "where to save result");

    writeResult(destinationName, result);

    delete[] result;
    deleteSource(source);

    return 0;
}
