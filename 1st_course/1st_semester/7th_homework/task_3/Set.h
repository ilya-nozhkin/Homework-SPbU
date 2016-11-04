#pragma once

#include <ostream>

struct Set;

Set *createSet();
void deleteSet(Set *&set);

void clear(Set *set);

bool add(Set *set, int value);
bool remove(Set *set, int value);
bool exists(Set *set, int value);

void printAscending(Set *set, std::ostream &stream);
void printDescending(Set *set, std::ostream &stream);
void serialize(Set *set, std::ostream &stream);

bool isBalanced(Set *set);
