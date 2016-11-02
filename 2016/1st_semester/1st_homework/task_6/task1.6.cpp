#include <stdio.h>
#include <cstring>

int main()
{
	printf("данная программа реализует нахождение кол-ва вхождений строки needle в строку haystack как подстроки\n");
	
	const int needleSize = 256;
	char needle[needleSize] = {0};
	printf("Введите строку needle:\n");
	fgets(needle, needleSize, stdin);
	int needleLength = strlen(needle);
	needleLength--;
	
	const int haystackSize = 1024;
	char haystack[haystackSize] = {0};
	printf("Введите строку haystack:\n");
	fgets(haystack, haystackSize, stdin);
	int haystackLength = strlen(haystack);
	
	int needlesCount = 0;
	for (int i = 0; i < haystackLength - needleLength; ++i)
	{
		int cursor = 0;
		while (cursor < needleLength && needle[cursor] == haystack[i + cursor])
		{
			cursor++;
		}
		if (cursor == needleLength) 
		{
			needlesCount++;
		}
	}
	
	printf("Строка needle входит в строку haystack %d раз(а)\n", needlesCount);
 
	return 0;
}
