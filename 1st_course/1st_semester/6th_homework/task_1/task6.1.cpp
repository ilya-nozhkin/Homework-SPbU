#include <iostream>
#include <cstring>

using namespace std;

const int fractionBits = 52;
const int exponentBits = 11;
const int numberSignBits = 1;
const int exponentOffset = 1023;

void normalizeNumber(char *left, int position)
{
    while (left[position] != '\0' && left[position] > '9')
    {
        if (left[position + 1] == '\0')
            left[position + 1] = '0';

        left[position + 1] += (left[position] - '0') / 10;
        left[position] = (left[position] - '0') % 10 + '0';

        position++;
    }
}

void longSum(char *left, const char *right)
{
    int leftLength = strlen(left);
    int rightLength = strlen(right);
    for (int i = 0; i < rightLength; i++)
    {
        int leftValue = left[i] - '0';
        int rightValue = right[i] - '0';
        int sum = leftValue + rightValue;
        left[i] = sum + '0';

        normalizeNumber(left, i);
    }

    if (rightLength >= leftLength)
        left[rightLength + 1] = 0;
}

void longMultipleWith5(char *number)
{
    int length = strlen(number);
    int store = 0;
    for (int i = 0; i < length; i++)
    {
        int value = number[i] - '0';
        value *= 5;
        number[i] = value % 10 + store + '0';
        store = value / 10;

        normalizeNumber(number, i);
    }

    if (store > 0)
    {
        if (number[length] == '\0')
            number[length] = '0';

        number[length] += store;
        normalizeNumber(number, length);
    }
}

void longMultipleWith10(char *number)
{
    int length = strlen(number);
    for (int i = length; i > 0; i--)
        number[i] = number[i - 1];
    number[0] = '0';
    number[length + 1] = '\0';
}

void printLongNumber(char *number, int point)
{
    int start = 0;
    while (number[start] != '\0' && number[start] == '0')
        start++;

    int length = strlen(number);
    int pointCounter = 0;
    for (int i = length - 1; i >= start; i--)
    {
        if (pointCounter == point)
            cout << '.';
        pointCounter++;

        cout << number[i];
    }
}

bool printSpecialDoubles(double value)
{
    uint64_t code = *reinterpret_cast<uint64_t*>(&value);
    
    const uint64_t null =             0x0000000000000000;
    const uint64_t nullNegative =     0x8000000000000000;
    const uint64_t infinity =         0x7ff0000000000000;
    const uint64_t infinityNegative = 0xfff0000000000000;
    const uint64_t nan =              0x7fffffffffffffff;
    
    if (code == null)
    {
        cout << "0.0";
        return true;
    }
    if (code == nullNegative)
    {
        cout << "-0.0";
        return true;
    }
    if (code == infinity)
    {
        cout << "+inf";
        return true;
    }
    if (code == infinityNegative)
    {
        cout << "-inf";
        return true;
    }
    if (code == nan)
    {
        cout << "NaN";
        return true;
    }
    
    return false;
}

uint64_t extractBits(uint64_t source, int start, int length)
{
    uint64_t mask = 0;
    for (int i = 0; i < length; i++)
        mask = (mask << 1) | 1;
    return (source >> start) & mask;
}

void printDouble(double value)
{
    if (printSpecialDoubles(value))
        return;
    
    uint64_t code = *reinterpret_cast<uint64_t*>(&value);

    uint64_t fraction = extractBits(code, 0, fractionBits);
    int64_t exponent = extractBits(code, fractionBits, exponentBits);
    uint64_t sign = extractBits(code, fractionBits + exponentBits, numberSignBits);

    char sum[64] = "01"; //10
    char current[64] = "5";

    for (int i = 0; i < fractionBits; i++)
    {
        uint64_t bit = fraction >> (fractionBits - i - 1);

        if ((bit & 1) == 1)
            longSum(sum, current);

        longMultipleWith10(sum);
        longMultipleWith5(current);
    }

    if (sign == 1)
        cout << '-';
    printLongNumber(sum, 1);
    cout << " * 2^" << (exponent - exponentOffset);
}

double enterDouble(const char *what)
{
    double value = 0.0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

int main()
{
    double value = enterDouble("a number");
    cout << "Result: ";
    printDouble(value);

    return 0;
}
