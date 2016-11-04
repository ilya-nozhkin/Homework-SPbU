#include <iostream>
#include <cstdlib>

using namespace std;

void swap(int &left, int &right)
{
    int temporary = left;
    left = right;
    right = temporary;
}

void quickSort(int *array, int first, int last)
{
    if (last - first <= 1)
    {
        if (array[last] < array[first])
        {
            swap(array[last], array[first]);
        }
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

void printArray(int *array, int size)
{
    for (int i = 0; i < size; i++)
        cout << array[i] << ' ';
    cout << endl;
}

int main()
{
    int sizeOfArray = 0;
    cout << "Enter the size of the array: ";
    cin >> sizeOfArray;
    
    int *array = new int[sizeOfArray];
    
    srand(time(nullptr));
    for (int i = 0; i < sizeOfArray; i++)
        array[i] = rand() % 90 + 10;
        
    cout << "Array has been filled with these random 2-digit numbers: " << endl;
    printArray(array, sizeOfArray);
    
    quickSort(array, 0, sizeOfArray - 1);
    
    cout << "Sorted array:" << endl;
    printArray(array, sizeOfArray);
    
    delete[] array;
    
    return 0;
}
