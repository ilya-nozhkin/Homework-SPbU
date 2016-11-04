#include <iostream>

using namespace std;

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

int* createArrayOfNumbers(int size)
{
    int *array = new int[size];
    for (int i = 0; i < size; i++)
        array[i] = 0;
    return array; 
}

void deleteArray(int *&array)
{
    delete[] array;
    array = nullptr;
}

unsigned int wideRand()
{
    //according to standard, RAND_MAX >= 32768 (2^15), that's why we should generate at least 2 numbers to 
    //get number >= 10^9
    //on the first step we get number                       00000000000000000xxxxxxxxxxxxxxx where x is a random bit
    //on the next step we shift number and get              00xxxxxxxxxxxxxxx000000000000000
    //and on the last step we add new random number and get 00xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    //so, maximum number is 2^30 - 1 that is greater than 10^9
    //P.S. 0x7FFF = 01111111 11111111
    return (((unsigned int) rand() & 0x7FFF) << 15) + 
            ((unsigned int) rand() & 0x7FFF);
}

void generateRandomNumbers(int *array, int size)
{
    const int max = 1000000000;
    for (int i = 0; i < size; i++)
        array[i] = wideRand() % max;
}

void printArray(int *array, int size, const char *what)
{
    const int skipSize = 100;
    if (size <= skipSize)
    {
        cout << what << endl;
        for (int i = 0; i < size; i++)
            cout << array[i] << ' ';
        cout << endl;
    }
    else
    {
        cout << what << " won't be printed because there are too many of them\n";
    }
}

void quickSort(int *array, int first, int last)
{
    if (last - first <= 1)
    {
        if (array[last] < array[first])
            swap(array[last], array[first]);
        return;
    }
    
    int pivot = array[(first + last) / 2];
    
    int left = first;
    int right = last;
    
    while (left <= right)
    {
        while (left <= right && array[left] < pivot)
            left++;
        
        while (left <= right && array[right] > pivot)
            right--;
            
        if (left <= right)
        {
            swap(array[left], array[right]);
            left++;
            right--;
        }
    }

    quickSort(array, first, right);
    quickSort(array, left,  last);
}

bool binarySearch(int *array, int first, int last, int element)
{
    if (first == last)
        return array[first] == element;
        
    bool result = false;
    int medium = (first + last) / 2;
    
    if (element < array[medium] && medium - 1 >= first)
        result = binarySearch(array, first, medium - 1, element);
        
    if (element > array[medium] && medium + 1 <= last)
        result = binarySearch(array, medium + 1, last, element);
        
    if (element == array[medium])
        result = true;
        
    return result;
}

int main()
{
    srand(time(nullptr));
    
    int n = inputNumber("n");
    int k = inputNumber("k");
    
    int *array = createArrayOfNumbers(n);
    generateRandomNumbers(array, n);
    printArray(array, n, "Array elements");
    
    int *numbers = createArrayOfNumbers(k);
    generateRandomNumbers(numbers, k);
    printArray(numbers, k, "Numbers");
    
    quickSort(array, 0, n - 1);
    
    for (int i = 0; i < k; i++)
        if (binarySearch(array, 0, n - 1, numbers[i]))
        {
            cout << "Array has number " << numbers[i] << " from number set" << endl;
        }
    
    deleteArray(numbers);
    deleteArray(array);
    
    return 0;
}
