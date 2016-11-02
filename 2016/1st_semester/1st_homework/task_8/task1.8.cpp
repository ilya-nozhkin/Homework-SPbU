#include <stdio.h>

typedef unsigned long long int uint64_t;

uint64_t recursiveFactorial(uint64_t number)
{
	if (number == 1)
	{
		return 1;
	}
	return number * recursiveFactorial(number - 1);
}

uint64_t iterativeFactorial(uint64_t number)
{
	uint64_t result = 1;
	while (number > 0)
	{
		result *= number;
		number--;
	}
	return result;
}

int main()
{
	printf("Данная программа реализует подсчет факториала\n");
	
	uint64_t number = 0;
	printf("Введите число, факториал которого необходимо получить: ");
	scanf("%llu", &number);
	
	printf("Факториал, полученный рекурсивным путем: %llu\n", recursiveFactorial(number));
	printf("Факториал, полученный итеративным путем: %llu\n", iterativeFactorial(number));
 
	return 0;
}
