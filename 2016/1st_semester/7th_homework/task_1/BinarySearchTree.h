#pragma once

#include <ostream>

struct BinarySearchTree;

BinarySearchTree *createBinarySearchTree();
void deleteTree(BinarySearchTree *&tree);

void clear(BinarySearchTree *tree);

bool add(BinarySearchTree *tree, int value);
bool remove(BinarySearchTree *tree, int value);
bool exists(BinarySearchTree *tree, int value);
int size(BinarySearchTree *tree);

void serialize(BinarySearchTree *tree, std::ostream &stream);

//Attention!!! iterators becomes invalid if tree is modified, you should delete all iterators before modifying the tree

struct BinarySearchTreeIterator;

BinarySearchTreeIterator *createBinarySearchTreeIterator(BinarySearchTree *tree, bool ascending);
void deleteIterator(BinarySearchTreeIterator *&iterator);

bool move(BinarySearchTreeIterator *iterator);
int getValue(BinarySearchTreeIterator *iterator);
