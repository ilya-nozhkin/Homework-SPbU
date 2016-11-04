#include <stdio.h>

int main()
{
    double x = 0.0;
    printf("данная программа расчитывает значение выражения\n"
           "x^4 + x^3 + x^2 + x + 1 за 2 умножения\n");
    printf("Введите число x: ");
    scanf("%lf", &x);
    
    //x^4 + x^3 + x^2 + x + 1 == (x^2 + x) * (x^2 + 1) + 1
    double xPow2 = x * x;
    double result = (xPow2 + x) * (xPow2 + 1) + 1;
    
    //вывод результата
    printf("x^4 + x^3 + x^2 + x + 1 = %lf\n", result);
    
    return 0;
}
