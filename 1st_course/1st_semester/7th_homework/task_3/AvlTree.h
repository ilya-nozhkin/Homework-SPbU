#pragma once

#include <ostream>

struct AvlTree;

AvlTree *createAvlTree();
void deleteTree(AvlTree *&tree);

void clear(AvlTree *tree);

bool add(AvlTree *tree, int value);
bool remove(AvlTree *tree, int value);
bool exists(AvlTree *tree, int value);
int size(AvlTree *tree);

void serialize(AvlTree *tree, std::ostream &stream);

bool isBalanced(AvlTree *tree);

//Attention!!! iterators become invalid if tree is modified, you should delete all iterators before modifying the tree

struct AvlTreeIterator;

AvlTreeIterator *createAvlTreeIterator(AvlTree *tree, bool ascending);
void deleteIterator(AvlTreeIterator *&iterator);

bool move(AvlTreeIterator *iterator);
int getValue(AvlTreeIterator *iterator);
