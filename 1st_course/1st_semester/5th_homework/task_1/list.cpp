#include "list.h"
#include <limits.h>

struct ListElement
{
    int value;
    ListElement *next;
};

ListElement *createListElement(int value, ListElement *next)
{
    ListElement *element = new ListElement;
    element->value = value;
    element->next = next;
    return element;
}

struct List
{
    ListElement *head;
    int size;
};

List *createList()
{
    List *list = new List;
    list->head = nullptr;
    list->size = 0;
    return list;
}

void deleteList(List *&list)
{
    clear(list);
    delete list;
    list = nullptr;
}

void add(List *list, int value)
{
    if (isEmpty(list))
    {
        list->head = createListElement(value, list->head);
        list->size++;
        return;
    }

    if (list->head->value > value)
    {
        list->head = createListElement(value, list->head);
        list->size++;
        return;
    }

    ListElement *cursor = list->head;

    while (cursor->next != nullptr && cursor->next->value < value)
        cursor = cursor->next;

    cursor->next = createListElement(value, cursor->next);
    list->size++;
}

void deleteElementFromList(ListElement *&element)
{
    ListElement *storedNext = element->next;
    delete element;
    element = storedNext;
}

bool remove(List *list, int value)
{
    if (size(list) == 0)
        return false;

    if (list->head->value == value)
    {
        deleteElementFromList(list->head);
        list->size--;
        return true;
    }

    ListElement *cursor = list->head;
    while (cursor->next != nullptr && cursor->next->value < value)
        cursor = cursor->next;

    if (cursor->next == nullptr)
        return false;

    if (cursor->next->value != value)
        return false;

    deleteElementFromList(cursor->next);
    list->size--;

    return true;
}

void clear(List *list)
{
    //function "deleteElementFromList" automatically redirects pointer to next element
    while (list->head != nullptr)
        deleteElementFromList(list->head);

    list->size = 0;
}

int size(List *list)
{
    return list->size;
}

bool isEmpty(List *list)
{
    return size(list) == 0;
}

int get(List *list, int index)
{
    if (size(list) <= index)
        return INT_MIN;

    ListElement *cursor = list->head;
    for (int i = 0; i < index; i++)
        cursor = cursor->next;

    return cursor->value;
}
