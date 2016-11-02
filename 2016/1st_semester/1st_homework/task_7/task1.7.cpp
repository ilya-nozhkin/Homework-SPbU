#include <stdio.h>
#include <math.h>

int main()
{
	printf("данная программа выводит все простые числа, не превосходящие заданного\n");
	
	int limit = 0;
	scanf("%d", &limit);
	
	//программа реализует решето Эратосфена
	
	bool *buffer = new bool[limit + 1];
	buffer[0] = buffer[1] = false;
	for (int i = 2; i <= limit; ++i)
	{
		buffer[i] = true;
	}
	
	int limitRoot = (int)sqrt(limit) + 1;
	for (int i = 2; i <= limitRoot; ++i)
	{
		if (buffer[i])
		{
			for (int j = i * i; j <= limit; j += i)
			{
				buffer[j] = false;
			}
		}
	}
	
	//вывод результата
	printf("Простые числа, не превышающие %d:\n", limit);
	for (int i = 2; i <= limit; ++i)
	{
		if (buffer[i])
		{
			printf("%d ", i);
		}
	}
	
	delete[] buffer;
 
	return 0;
}
