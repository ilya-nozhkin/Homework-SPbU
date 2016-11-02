#include <iostream>
#include "circularList.h"

using namespace std;

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

int main()
{
    int sicarii = inputNumber("number of sikarii");
    int step = inputNumber("step (for example if every third should be killed then step = 2)");

    CircularList *list = createCircularList();

    for (int i = 1; i <= sicarii; i++)
        add(list, i);

    Iterator *iterator = createIterator(list, 0);
    while (size(list) > 1)
    {
        moveIterator(iterator, step);
        removeByIterator(iterator);
    }

    cout << "last sikarius has number " << getByIterator(iterator);

    deleteIterator(iterator);
    deleteCircularList(list);

    return 0;
}
