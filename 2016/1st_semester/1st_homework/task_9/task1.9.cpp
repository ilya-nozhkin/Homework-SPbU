#include <stdio.h>

int main()
{
	printf("Данная программа реализует подсчет целой степени n числа a\n");
	
	int a = 0;
	int n = 0;
	printf("Введите числа a и n через пробел: ");
	scanf("%d %d", &a, &n);
	
	int result = n == 0 ? 1 : a; //проверка на a^0
	for (int i = 1; i < n; ++i)
	{
		result *= a;
	}
	
	//вывод результата
	printf("%d^%d = %d\n", a, n, result);
 
	return 0;
}
