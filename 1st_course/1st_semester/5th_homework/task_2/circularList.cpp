#include "circularList.h"
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

struct CircularList
{
    ListElement *head;
    int size;
};

CircularList *createCircularList()
{
    CircularList *list = new CircularList;
    list->head = nullptr;
    list->size = 0;
    return list;
}

void deleteCircularList(CircularList *&list)
{
    clear(list);
    delete list;
    list = nullptr;
}

ListElement *findPreviousElement(ListElement *element)
{
    ListElement *cursor = element;
    while (cursor->next != element)
        cursor = cursor->next;
    return cursor;
}

void add(CircularList *list, int value)
{
    if (isEmpty(list))
    {
        list->head = createListElement(value, nullptr);
        list->head->next = list->head;
        list->size++;
        return;
    }

    if (list->head->value > value)
    {
        ListElement *previous = findPreviousElement(list->head);
        list->head = createListElement(value, list->head);
        previous->next = list->head;
        list->size++;
        return;
    }

    ListElement *cursor = list->head;

    while (cursor->next != list->head && cursor->next->value < value)
        cursor = cursor->next;

    cursor->next = createListElement(value, cursor->next);
    list->size++;
}

void deleteElementFromList(ListElement *&element)
{
    ListElement *previous = findPreviousElement(element);
    ListElement *storedNext = element->next;
    delete element;
    element = storedNext;
    previous->next = element;
}

bool remove(CircularList *list, int value)
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
    while (cursor->next != list->head && cursor->next->value < value)
        cursor = cursor->next;

    if (cursor->next == list->head)
        return false;

    if (cursor->next->value != value)
        return false;

    deleteElementFromList(cursor->next);
    list->size--;

    return true;
}

void clear(CircularList *list)
{
    if (isEmpty(list))
        return;

    ListElement *cursor = list->head->next;
    while (cursor != list->head)
    {
        ListElement *toDelete = cursor;
        cursor = cursor->next;
        delete toDelete;
    }

    delete list->head;
    list->head = nullptr;

    list->size = 0;
}

int size(CircularList *list)
{
    return list->size;
}

bool isEmpty(CircularList *list)
{
    return size(list) == 0;
}

struct Iterator
{
    CircularList *list;
    ListElement *element;
};

Iterator *createIterator(CircularList *list, int startIndex)
{
    if (list->head == nullptr)
        return nullptr;

    Iterator *iterator = new Iterator;
    iterator->element = list->head;
    iterator->list = list;

    moveIterator(iterator, startIndex);

    return iterator;
}

void deleteIterator(Iterator *&iterator)
{
    delete iterator;
    iterator = nullptr;
}

void moveIterator(Iterator *iterator, int steps)
{
    for (int i = 0; i < steps; i++)
        iterator->element = iterator->element->next;
}

int getByIterator(Iterator *iterator)
{
    return iterator->element->value;
}

void removeByIterator(Iterator *iterator)
{
    ListElement *storedPointer = iterator->element;

    deleteElementFromList(iterator->element);

    if (storedPointer == iterator->list->head)
        iterator->list->head = iterator->element;

    iterator->list->size--;
}
