#pragma once

struct CircularList;
struct Iterator;

CircularList *createCircularList();
void deleteCircularList(CircularList *&list);

void add(CircularList *list, int value);
bool remove(CircularList *list, int value);
void clear(CircularList *list);

int size(CircularList *list);
bool isEmpty(CircularList *list);



Iterator *createIterator(CircularList *list, int startIndex);
void deleteIterator(Iterator *&iterator);

void moveIterator(Iterator *iterator, int steps);
int getByIterator(Iterator *iterator);
void removeByIterator(Iterator *iterator);
