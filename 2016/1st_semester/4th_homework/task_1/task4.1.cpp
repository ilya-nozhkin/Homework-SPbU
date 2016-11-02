#include <iostream>

using namespace std;

void swap(int &left, int &right)
{
	int temporary = left;
	left = right;
	right = temporary;
}

int inputNumber(const char *what)
{
	int value = 0;
	cout << "Enter " << what << ": ";
	cin >> value;
	return value;
}

int **createArray2D(int size)
{
	int **array = new int*[size];
	
	for (int i = 0; i < size; i++)
	{
		array[i] = new int[size];
		for (int j = 0; j < size; j++)
			array[i][j] = 0;
	}
	
	return array;
}

void deleteArray(int **&array, int size)
{
	for (int i = 0; i < size; i++)
		delete[] array[i];
	delete[] array;
	array = nullptr;
}

void generateRandomNumbers(int **array, int size)
{
	const int max = 99;
	const int min = 10;

	srand(time(nullptr));	
	for (int i = 0; i < size; i++)
		for (int j = 0; j < size; j++)
			array[i][j] = rand() % (max - min + 1) + min;
}

void printArray2D(int **array, int size)
{
	for (int i = 0; i < size; i++)
	{
		for (int j = 0; j < size; j++)
			cout << array[i][j] << ' ';
		cout << endl;
	}
}

void printArraySpirally(int **array, int size)
{
	int row = size / 2;
	int column = size / 2; 
	int step = 0;
	int length = 1;
	int deltaColumn = 1;
	int deltaRow = 0;
	int stage = 0;
	
	while(row    >= 0 && row    < size && 
	      column >= 0 && column < size)
	{
		cout << array[column][row] << ' ';
		
		row    += deltaRow;
		column += deltaColumn;
		
		step++;
		
		if (step == length)
		{
			step = 0;
			stage++;
			if (stage % 2 == 0)
				length++;
			swap(deltaRow, deltaColumn);
			deltaRow = -deltaRow;
		}
	}
	
	cout << endl;
}

int main()
{
	int size = inputNumber("size of array");
	int **numbers = createArray2D(size);
	generateRandomNumbers(numbers, size);
	
	cout << "program has filled array with random 2-digit numbers:" << endl;
	printArray2D(numbers, size);
	
	cout << "array which is printed spirally:" << endl;
	printArraySpirally(numbers, size);
	
	deleteArray(numbers, size);
	
	return 0;
}
