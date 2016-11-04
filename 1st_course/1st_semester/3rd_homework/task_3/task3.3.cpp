#include <iostream>
#include <cstring>

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

void countChars(const char *string, int sums[256])
{
    int length = strlen(string);
    for (int i = 0; i < length; i++)
        sums[(int) string[i]]++;
}

bool compareSums(int first[256], int second[256])
{
    for (int i = 0; i < 256; i++)
        if (first[i] != second[i])
            return false;
            
    return true;
}

bool areStringsTransformable(const char *firstString, const char *secondString)
{
    int firstSums[256] = {0};
    int secondSums[256] = {0};
    
    countChars(firstString, firstSums);
    countChars(secondString, secondSums);
    
    return compareSums(firstSums, secondSums);
}

void outputResult(bool transformable)
{
    if (transformable)
    {
        cout << "you can transform first string to second moving characters" << endl;
    }
    else
    {
        cout << "you can not transform first string to second moving characters" << endl;
    }
}

int main()
{
    const int stringSize = 1024;
    char firstString[stringSize] = {0};
    char secondString[stringSize] = {0};
    inputString(firstString, "the first string");
    inputString(secondString, "the second string");
    
    bool transformable = areStringsTransformable(firstString, secondString);
    
    outputResult(transformable);
    
    return 0;
}
