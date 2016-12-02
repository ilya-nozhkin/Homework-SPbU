#include <iostream>
#include <cstring>
#include <inttypes.h>

using namespace std;

struct RabinKarpResult
{
    int *entries;
    int numEntries;
};

void deleteResult(RabinKarpResult *&result)
{
    delete[] result->entries;
    delete result;
    result = nullptr;
}

uint64_t mod(int64_t value, int64_t modulo)
{
    return (uint64_t) ((value % modulo + modulo) % modulo);
}

void hashStep(uint64_t &code, char newChar, char oldChar, uint64_t factor, uint64_t lastPower, uint64_t modulo)
{
    code = mod(factor * ((int64_t) code - lastPower * oldChar) + newChar, modulo);
}

uint64_t power(uint64_t left, uint64_t right, uint64_t modulo)
{
    if (right == 0)
        return 1;

    uint64_t result = left % modulo;
    while (right > 1)
    {
        result *= left;
        result %= modulo;
        right--;
    }

    return result;
}

uint64_t stringHash(const char *string, int length, uint64_t factor, uint64_t modulo)
{
    uint64_t code = 0;

    for (int i = 0; i < length; i++)
    {
        code *= factor;
        code += string[i];
        code %= modulo;
    }

    return code;
}

RabinKarpResult *rabinKarpSearch(const char *haystack, const char *needle)
{
    const int modulo = 1000000007;
    const int factor = 127;

    int haystackLength = strlen(haystack);
    int needleLength = strlen(needle);

    RabinKarpResult *result = new RabinKarpResult;
    result->numEntries = 0;

    if (haystackLength < needleLength)
        return result;

    result->entries = new int[haystackLength];

    uint64_t lastPower = power(factor, needleLength - 1, modulo);

    uint64_t needleCode = stringHash(needle, needleLength, factor, modulo);
    uint64_t currentCode = stringHash(haystack, needleLength, factor, modulo);

    int cursor = 0;
    while (cursor + needleLength <= haystackLength)
    {
        if (needleCode == currentCode)
        {
            if (strncmp(haystack + cursor, needle, needleLength) == 0)
            {
                result->entries[result->numEntries] = cursor;
                result->numEntries++;
            }
        }

        if (cursor + needleLength < haystackLength)
        {
            char newChar = haystack[cursor + needleLength];
            char oldChar = haystack[cursor];
            hashStep(currentCode, newChar, oldChar, factor, lastPower, modulo);
        }

        cursor++;
    }

    return result;
}

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";

    cin >> buffer;
}

int main()
{
    const int haystackSize = 1024;
    const int needleSize = 256;

    char haystack[haystackSize] = {'\0'};
    char needle[needleSize] = {'\0'};

    inputString(haystack, "haystack");
    inputString(needle, "needle");

    RabinKarpResult *result = rabinKarpSearch(haystack, needle);

    cout << "There are(is) " << result->numEntries << " entries: ";
    for (int i = 0; i < result->numEntries; i++)
        cout << result->entries[i] << (i < result->numEntries - 1 ? "," : "");
    cout << endl;

    deleteResult(result);

    return 0;
}
