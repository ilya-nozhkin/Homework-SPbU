#pragma once

#include <istream>
#include <ostream>

struct CalculatingTree;

CalculatingTree *createCalculatingTree();
void deleteTree(CalculatingTree *&tree);

int calculate(CalculatingTree *tree);

void parse(CalculatingTree *tree, std::istream &stream);
void printInfixForm(CalculatingTree *tree, std::ostream &stream);
