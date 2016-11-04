#pragma once

struct List;

List *createList();
void deleteList(List *&list);

void add(List *list, int value);
bool remove(List *list, int value);
void clear(List *list);

int size(List *list);
bool isEmpty(List *list);
int get(List *list, int index);

