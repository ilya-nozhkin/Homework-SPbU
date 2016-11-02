#include <iostream>
#include <cstdlib>

using namespace std;

void swap(int &left, int &right)
{
	int temporary = left;
	left = right;
	right = temporary;
}

void orderElement(int *array, int size, int index)
{
	bool process = true;
	while (index * 2 + 1 < size && process)
	{
		int firstChild = index * 2;
		int secondChild = firstChild + 1;
		int maxChild = array[secondChild] > array[firstChild] ? secondChild : firstChild;
		
		if (array[maxChild] > array[index])
		{
			swap(array[index], array[maxChild]);
			index = maxChild;
		}
		else
		{
			process = false;
		}
	}
}

void heapSort(int *array, int size)
{
	for (int i = size / 2; i >= 0; i--)
		orderElement(array, size, i);

	for (int i = size - 1; i > 0; i--)
	{
		swap(array[0], array[i]);
		orderElement(array, i, 0);
	}
}

void printArray(int *array, int size)
{
	for (int i = 0; i < size; i++)
		cout << array[i] << ' ';
	cout << endl;
}

bool checkOrder(int *array, int size)
{
	for (int i = 0; i < size - 1; i++)
		if (array[i] > array[i + 1])
			return false;
			
	return true;
}

void generateRandomNumbers(int *array, int size)
{
	const int numbersSize = 1000;
	
	for (int i = 0; i < size; i++)
		array[i] = rand() % numbersSize;
}

bool automaticRandomTest()
{
	const int maxQuantity = 8192;
	
	int size = rand() % maxQuantity + 1;
	int *array = new int[size];
	
	generateRandomNumbers(array, size);
		
	heapSort(array, size);
	
	bool sortingIsCorrect = checkOrder(array, size);
	if (!sortingIsCorrect)
	{
		cout << "Heap sorting gave an incorrect result: " << endl;
		printArray(array, size);
	}
	
	delete[] array;
	
	return sortingIsCorrect;
}

bool doAutomaticTests(int quantity)
{
	for (int i = 0; i < quantity; i++)
	{
		if (!automaticRandomTest())
			return false;
	}
	return true;
}

int inputNumber(const char *what)
{
	int number = 0;
	cout << "Enter " << what << ": ";
	cin >> number;
	
	return number;
}

void inputArray(int *array, int size)
{
	cout << "Enter your array:" << endl;
	for (int i = 0; i < size; i++)
		cin >> array[i];
}

int main()
{
	const int automaticTests = 200;
	srand(time(nullptr));
	if (doAutomaticTests(automaticTests))
		cout << automaticTests << " automatic random tests have been performed correctly" << endl;
	
	cout << "You can perform your own test" << endl;
	
	int size = inputNumber("the size of your array");
	
	int *array = new int[size];
	inputArray(array, size);

	heapSort(array, size);

	cout << "Sorted array:" << endl;
	printArray(array, size);

	delete[] array;

	return 0;
}
