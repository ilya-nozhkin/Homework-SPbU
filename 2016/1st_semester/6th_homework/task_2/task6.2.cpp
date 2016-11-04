#include <iostream>
#include <cstring>

using namespace std;

int32_t inputNumber(const char *what)
{
    int32_t value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

const int bitness = 32;
const int bufferSize = bitness + 1;

typedef char int32str[bufferSize];

void binarySum(int32str result, const int32str left, const int32str right)
{
    int cursor = bitness - 1;
    
    int store = 0;
    while (cursor >= 0)
    {
        int leftValue = left[cursor] - '0';
        int rightValue = right[cursor] - '0';
            
        int resultValue = leftValue + rightValue + store;
        store = resultValue / 2;
        resultValue %= 2;
        
        result[cursor] = resultValue + '0';
        
        cursor--;
    }
}

void binaryInverse(int32str number)
{
    for (int i = 0; i < bitness; i++)
        number[i] = 1 - (number[i] - '0') + '0';
}

void decimalToBinary(int32str result, int32_t number)
{
    bool negative = number < 0;
    if (negative)
        number = -number;
        
    result[bitness] = 0;
    int cursor = bitness - 1;
    while (cursor >= 0)
    {
        result[cursor] = number % 2 + '0';
        
        number /= 2;
        cursor--;
    }
    
    if (negative)
    {
        binaryInverse(result);
        
        int32str buffer = {0};
        const int32str one = "00000000000000000000000000000001";
        
        strcpy(buffer, result);
        binarySum(result, buffer, one);
    }
}

int32_t binaryToDecimal(int32str number)
{
    uint32_t result = 0;
    uint32_t counter = 1;
    
    for (int i = bitness - 1; i >= 1; i--)
    {
        if (number[i] == '1')
            result += counter;
        
        counter *= 2;
    }
    
    if (number[0] == '1')
        result = -(0x7FFFFFFF - (result - 1));
            
    return result;
}

void printString(const char *string, int maxLength)
{
    int length = strlen(string);
    int min = length < maxLength ? length : maxLength;
    
    for (int i = 0; i < min; i++)
        cout << string[i];
    
    for (int j = min; j < maxLength; j++)
        cout << ' ';
}

//I think that this task requires to operate numbers without bitwise operations because it is too easy to
//copy bits from number to string and back. That's why I use basic arithmetic operations anywhere.
//However if it is not true you should describe tasks in more detail ;)
int main()
{
    const int offset = 32;
    
    int32_t first = inputNumber("the first number");
    int32_t second = inputNumber("the second number");
    
    int32str firstBinary = {0};
    decimalToBinary(firstBinary, first);
    printString("first number in binary format:", offset);
    cout << firstBinary << endl;
    
    int32str secondBinary = {0};
    decimalToBinary(secondBinary, second);
    printString("second number in binary format:", offset);
    cout << secondBinary << endl;
    
    int32str sum = {0};
    binarySum(sum, firstBinary, secondBinary);
    printString("sum in binary format:", offset);
    cout << sum << endl;
    
    printString("sum in decimal format:", offset);
    cout << binaryToDecimal(sum) << endl;
        
    return 0;
}
