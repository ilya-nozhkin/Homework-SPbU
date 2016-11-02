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

void deleteArray(int *array)
{
	delete[] array;
}

void inputArray(int *array, int size)
{
	cout << "Enter elements of the array:" << endl;
	for (int i = 0; i < size; i++)
		cin >> array[i];
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

bool findMaxMultipleElement(int *array, int size, int &max)
{
	quickSort(array, 0, size - 1);
	
	max = 0;
	bool found = false;
	
	for (int i = 0; i < size - 1; i++)
	{
		if (array[i] == array[i + 1])
		{
			if (!found || (array[i] > max))
			{
				max = array[i];
				found = true;
			}
		}
	}
	
	return found;
}

void outputResult(bool found, int max)
{
	if (found)
	{
		cout << "Maximum element which is found in the array more than once = " << max << endl;
	}
	else
	{
		cout << "There are not elements in the array which are found more than once" << endl;
	}
}

int main()
{
	int size = inputNumber("the size of array");
	int *array = createArrayOfNumbers(size);
	inputArray(array, size);
	
	int maxMultipleElement = 0;
	bool found = findMaxMultipleElement(array, size, maxMultipleElement);
	
	deleteArray(array);
	
	outputResult(found, maxMultipleElement);
	
	return 0;
}
