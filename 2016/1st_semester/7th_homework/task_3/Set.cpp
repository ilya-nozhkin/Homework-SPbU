#include "Set.h"
#include "AvlTree.h"

using namespace std;

struct Set
{
    AvlTree *tree;
};

Set *createSet()
{
    Set *set = new Set;
    set->tree = createAvlTree();
    return set;
}

void deleteSet(Set *&set)
{
    deleteTree(set->tree);
    delete set;
    set = nullptr;
}

void clear(Set *set)
{
    clear(set->tree);
}

bool add(Set *set, int value)
{
    return add(set->tree, value);
}

bool remove(Set *set, int value)
{
    return remove(set->tree, value);
}

bool exists(Set *set, int value)
{
    return exists(set->tree, value);
}

void printWithIterator(AvlTreeIterator *iterator, ostream &stream)
{
    if (iterator == nullptr)
    {
        stream << "set is empty";
        return;
    }

    stream << "elements of set: " << endl;
    stream << getValue(iterator) << ' ';

    while (move(iterator))
         stream << getValue(iterator) << ' ';
}

void printAscending(Set *set, std::ostream &stream)
{
    AvlTreeIterator *iterator = createAvlTreeIterator(set->tree, true);

    printWithIterator(iterator, stream);

    if (iterator != nullptr)
        deleteIterator(iterator);
}

void printDescending(Set *set, std::ostream &stream)
{
    AvlTreeIterator *iterator = createAvlTreeIterator(set->tree, false);

    printWithIterator(iterator, stream);

    if (iterator != nullptr)
        deleteIterator(iterator);
}

void serialize(Set *set, std::ostream &stream)
{
    serialize(set->tree, stream);
}

bool isBalanced(Set *set)
{
    return isBalanced(set->tree);
}
