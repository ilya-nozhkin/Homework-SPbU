#include <iostream>
#include <limits.h>
#include <fstream>
#include <cstring>

#include "hashMap.h"

using namespace std;

String *inputString(const char *what)
{
    const int size = 1024;
    char buffer[size] = {'\0'};
    cout << "Enter " << what << ": ";
    cin >> buffer;
    return createString(buffer);
}

String *encode(int number)
{
    char buffer[8] = {'\0'};

    for (int i = 0; i < 7; i++)
    {
        buffer[i] = number % (26 - i) + 'a' + i;
        number /= (26 - i);
    }

    buffer[7] = 0;

    return createString(buffer);
}

bool simpleTest()
{
    HashMap *map = createHashMap(&hashDJB2);

    const int initialSize = 1000;
    const int eraseStart = 200;
    const int eraseEnd = 400;

    for (int i = 0; i < initialSize; i++)
    {
        String *key = encode(i);
        add(map, key, i);
        deleteString(key);
    }

    for (int i = eraseStart; i <= eraseEnd; i++)
    {
        String *key = encode(i);
        remove(map, key);
        deleteString(key);
    }

    bool status = true;
    for (int i = 0; i < initialSize; i++)
    {
        String *key = encode(i);
        int value = getValue(map, key);

        if (i >= eraseStart && i <= eraseEnd)
        {
            if (value != INT_MIN)
                status = false;
        }
        else
        {
            if (value != i)
                status = false;
        }

        deleteString(key);

        if (!status)
            break;
    }

    deleteHashMap(map);

    return status;
}

HashFunction *chooseHashFunction()
{
    cout << "Choose one of these hash functions:" << endl;
    cout << "0 - sum of ascii codes" << endl;
    cout << "1 - some strange function with xor that has been found on stackoverflow" << endl;
    cout << "2 - djb2" << endl;
    cout << "3 - sdbm" << endl;
    cout << "> ";

    int code = 0;
    cin >> code;

    if (code == 0)
        return &hashSum;
    if (code == 1)
        return &hashXor;
    if (code == 2)
        return &hashDJB2;
    if (code == 3)
        return &hashSDBM;

    cout << "Unknown code, djb2 will be used" << endl;

    return hashDJB2;
}

void extractWord(char *buffer)
{
    int length = strlen(buffer);
    for (int i = 0; i < length; i++)
    {
        if (buffer[i] >= 'A' && buffer[i] <= 'Z')
            buffer[i] = buffer[i] - ('A' - 'a');
        else
        {
            if ((buffer[i] < 'a' || buffer[i] > 'z') && buffer[i] != '-' && buffer[i] != '\'')
                buffer[i] = '\0';
        }
    }
}

void readFile(HashMap *map, ifstream &file)
{
    while (!file.eof())
    {
        const int bufferSize = 256;
        char buffer[bufferSize] = {'\0'};

        file >> buffer;
        extractWord(buffer);

        int value = getValue(map, buffer);
        if (value == INT_MIN)
            add(map, buffer, 1);
        else
            setValue(map, buffer, value + 1);
    }
}

void outputResults(HashMap *map)
{
    float factor = loadFactor(map);
    float averageChainSize = averageChainLength(map);
    int   maximumChainSize = maximumChainLength(map);

    String **keys = new String*[maximumChainSize];
    int *values = new int[maximumChainSize];

    getLongestChain(map, keys, values);

    int addedWords = size(map);
    int emptyChains = emptyChainsNumber(map);

    cout << "Load factor: " << factor << endl;
    cout << "Average not empty chains length: " << averageChainSize << endl;
    cout << "Maximum chain length: " << maximumChainSize << endl;

    cout << "Longest chain: " << endl;
    for (int i = 0; i < maximumChainSize; i++)
        cout << castToChars(keys[i]) << " - " << values[i] << endl;

    cout << "Added words: " << addedWords << endl;
    cout << "Empty chains: " << emptyChains << endl;

    for (int i = 0; i < maximumChainSize; i++)
        deleteString(keys[i]);

    delete[] keys;
    delete[] values;
}

int main()
{
    bool testStatus = simpleTest();
    if (!testStatus)
    {
        cout << "Test has failed" << endl;
        return 1;
    }

    HashFunction *hash = chooseHashFunction();

    HashMap *map = createHashMap(hash);

    String *fileName = inputString("the name of file");
    ifstream file(castToChars(fileName));
    deleteString(fileName);

    if (file.is_open())
    {
        readFile(map, file);
        outputResults(map);

        file.close();
    }
    else
    {
        cout << "Couldn't open the file" << endl;
    }

    deleteHashMap(map);

    return 0;
}
