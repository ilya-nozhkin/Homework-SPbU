#include <stdio.h>

//функция, которая проверяет, является ли текущий символ закрывающей скобкой, и если является, то проверить, 
//является ли последний символ в стеке соответствующей открывающей скобкой
bool checkChar(char *stack, int &stackCursor, char current, char parOpen, char parClose)
{
    if (current == parClose)
    {
        if (stackCursor > 0)
        {
            //если попалась соответствующая скобка, то вернуть true и выбросить последний элемент
            return stack[--stackCursor] == parOpen;
        }
        else
        {
            return false;
        }
    }
    return true;
}

int main()
{
    printf("Данная программа реализует проверку баланса скобок\n"
           "в строке, при этом проверяются скобки: (), {}, []\n");
    
    //стек, в который будем помещать открывающие скобки и удалять их оттуда, если встретится закрывающая
    const int stackSize = 256;
    char stack[stackSize] = {0};
    int stackCursor = 0;
    
    const int maxStringSize = 1024;
    char string[maxStringSize] = {0};
    printf("Введите строку, которую необходимо проверить:\n");
    fgets(string, maxStringSize, stdin); //gets использовать опасно, поэтому она заменена fgets
    
    int i = 0;
    while (string[i] != '\0')
    {
        char current = string[i++];
        
        if (current == '(' || current == '{' || current == '[')
        {
            stack[stackCursor++] = current;
        }
        //проверить, не является ли текущий символ закрывающей скобкой и не нарушился ли баланс
        else if (!checkChar(stack, stackCursor, current, '(', ')') ||
                 !checkChar(stack, stackCursor, current, '{', '}') ||
                 !checkChar(stack, stackCursor, current, '[', ']'))
        {
            break;
        }
    }
    
    //баланс не нарушен, если курсор дошел до конца строки и к этому времени стек полностью очистился
    bool result = (stackCursor == 0) && (string[i] == '\0');
    printf("Баланс %sсоблюдается", result ? "" : "не ");
    
    return 0;
}
