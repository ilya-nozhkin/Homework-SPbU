#include <iostream>

using namespace std;

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

void countDigits(int counts[10], int number)
{
    while (number > 0)
    {
        counts[number % 10]++;
        number /= 10;
    }
}

void printMinimum(int number)
{
    int counts[10] = {0};
    countDigits(counts, number);
    
    bool found = false;
    int k = 1;
    while (!found && k < 10)
    {
        if (counts[k] > 0)
        {
            cout << k;
            counts[k]--;
            found = true;
        }
        k++;
    }
    
    if (found)
    {
        for (int i = 0; i < 10; i++)
            for (int j = 0; j < counts[i]; j++)
            {
                cout << i;
            }
        cout << endl;
    }
    else
    {
        cout << 0 << endl;
    }
}

int main()
{
    int number = inputNumber("number");
    printMinimum(number);
    
    return 0;
}
