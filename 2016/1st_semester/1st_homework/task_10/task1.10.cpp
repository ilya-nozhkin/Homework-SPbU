#include <stdio.h>
#include <cstring>

int main()
{
	printf("Данная программа реализует проверку строки, является ли она палиндромом\n");
	
	const int maxStringLength = 1024;
	char string[maxStringLength] = {0};
	printf("Введите строку: ");
	fgets(string, maxStringLength, stdin);
	
	int length = strlen(string);
	string[--length] = 0; //удалить символ перевода строки
	
	//left, right - итераторы, проходящие по строке с разных концов, если до момента их встречи символы, на которые они
	//указывали, всегда совпадали, то строка - палиндром
	int left = 0;
	int right = length - 1;
	while((left < right) && (string[left] == string[right]))
	{
		left++;
		right--;
	}
	bool isPalyndrome = left >= right;
 
	//вывод результата
	printf("Строка %sявляется палиндромом\n", isPalyndrome ? "" : "не ");
 
	return 0;
}
