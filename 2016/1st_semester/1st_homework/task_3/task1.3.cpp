#include <stdio.h>
#include <cstdlib> //для функции rand
#include <time.h> //для получения времени

//фукнкция, переворачивающая заданный участок переданного массива
void reverse(int *array, int begin, int range)
{
    for (int i = 0; i < range / 2; ++i)
    {
        int left  = begin + i;
        int right = begin + range - i - 1;
        
        int stored   = array[left];
        array[left]  = array[right];
        array[right] = stored;
    }
}

//функция, печатающая заданный участок массива
void printArray(int *array, int begin, int range)
{
    for (int i = begin; i < begin + range; ++i)
    {
        printf("%d ", array[i]);
    }
    putchar('\n');
}

int main()
{
    printf("Данная программа реализует обмен местами кусков массива\n"
           "с длинами m и n соответственно\n");
    
    int m = 0;
    int n = 0;
    printf("Введите через пробел размеры m и n кусков массива\n"
           "(массив будет заполнен случайными двузначными числами): ");
    scanf("%d %d", &m, &n);
    
    //из-за условия задачи нулевой элемент 
    //придется оставить неиспользуемым
    int arraySize = m + n;
    int *x = new int[arraySize + 1];
    
    //для удобства заполним массив случайными двузначными числами
    //инициализируем генератор текущим временем, чтобы числа всегда
    //получались разными
    srand(time(NULL)); 
    for (int i = 1; i <= arraySize; ++i)
    {
        x[i] = rand() % 90 + 10;
    }
    
    //выведем исходный массив, чтобы пользователю было с чем сравнивать
    printf("Исходный массив:\n");
    printArray(x, 1, arraySize);
    
    //произвести перестановку, описанную в условии можно, если
    //сначала перевернуть куски массива а затем и весь массив
    reverse(x, 1,     m);
    reverse(x, 1 + m, n);
    reverse(x, 1,     arraySize);
    
    //вывод результата
    printf("массив с перестановленными кусками:\n");
    printArray(x, 1, arraySize);
    
    return 0;
}
