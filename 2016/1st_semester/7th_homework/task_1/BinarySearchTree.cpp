#include "BinarySearchTree.h"

using namespace std;

struct Node
{
    int value;
    Node *left;
    Node *right;
};

struct BinarySearchTree
{
    Node *root;
    int size;
};

BinarySearchTree *createBinarySearchTree()
{
    BinarySearchTree *tree = new BinarySearchTree;
    tree->root = nullptr;
    tree->size = 0;

    return tree;
}

void deleteTree(BinarySearchTree *&tree)
{
    clear(tree);

    delete tree;
    tree = nullptr;
}

void deleteNode(Node *&node)
{
    if (node->left != nullptr)
        deleteNode(node->left);
    if (node->right != nullptr)
        deleteNode(node->right);

    delete node;
    node = nullptr;
}

void clear(BinarySearchTree *tree)
{
    if (tree->root != nullptr)
        deleteNode(tree->root);
}

Node *createNode(int value)
{
    Node *node = new Node;
    node->value = value;
    node->left = nullptr;
    node->right = nullptr;
    return node;
}

bool add(Node *&node, int value)
{
    if (node == nullptr)
    {
        node = createNode(value);
        return true;
    }

    if (value > node->value)
    {
        return add(node->right, value);
    }
    else if (value < node->value)
    {
        return add(node->left, value);
    }

    return false;
}

bool add(BinarySearchTree *tree, int value)
{
    bool status = add(tree->root, value);
    if (status)
        tree->size++;

    return status;
}

bool tryToRemoveLeftEmpty(Node *&node)
{
    if (node->left != nullptr)
        return false;

    Node *toDelete = node;
    node = node->right;
    delete toDelete;

    return true;
}

bool tryToRemoveRightEmpty(Node *&node)
{
    if (node->right != nullptr)
        return false;

    Node *toDelete = node;
    node = node->left;
    delete toDelete;

    return true;
}

void removeNonEmpty(Node *&node)
{
    Node *parent = node;
    Node *cursor = node->left;

    if (cursor->right == nullptr)
    {
        node->value = cursor->value;
        parent->left = cursor->left;
        delete cursor;
        return;
    }

    while (cursor->right != nullptr)
    {
        parent = cursor;
        cursor = cursor->right;
    }

    node->value = cursor->value;
    tryToRemoveRightEmpty(parent->right);
}

bool remove(Node *&node, int value)
{
    if (node == nullptr)
        return false;

    if (value > node->value)
    {
        return remove(node->right, value);
    }
    else if (value < node->value)
    {
        return remove(node->left, value);
    }

    if (!tryToRemoveLeftEmpty(node))
    {
        if (!tryToRemoveRightEmpty(node))
        {
            removeNonEmpty(node);
        }
    }

    return true;
}

bool remove(BinarySearchTree *tree, int value)
{
    bool status = remove(tree->root, value);
    if (status)
        tree->size--;

    return status;
}

bool exists(Node *node, int value)
{
    if (node == nullptr)
        return false;

    if (value > node->value)
    {
        return exists(node->right, value);
    }
    if (value < node->value)
    {
        return exists(node->left, value);
    }

    return true;
}

bool exists(BinarySearchTree *tree, int value)
{
    return exists(tree->root, value);
}

void serialize(Node *node, ostream &stream)
{
    if (node == nullptr)
        stream << "null";
    else
    {
        stream << '(' << node->value << ' ';
        serialize(node->left, stream);
        stream << ' ';
        serialize(node->right, stream);
        stream << ')';
    }
}

void serialize(BinarySearchTree *tree, ostream &stream)
{
    serialize(tree->root, stream);
}

int size(BinarySearchTree *tree)
{
    return tree->size;
}

struct BinarySearchTreeIterator
{
    Node **sequence;
    int length;
    int position;
    bool ascending;
};

void add(Node **sequence, int &size, Node *node)
{
    sequence[size] = node;
    size++;
}

void buildSequence(Node *node, Node **sequence, int &offset)
{
    if (node->left != nullptr)
        buildSequence(node->left, sequence, offset);

    sequence[offset] = node;
    offset++;

    if (node->right != nullptr)
        buildSequence(node->right, sequence, offset);
}

BinarySearchTreeIterator *createBinarySearchTreeIterator(BinarySearchTree *tree, bool ascending)
{
    if (tree->root == nullptr)
        return nullptr;

    BinarySearchTreeIterator *iterator = new BinarySearchTreeIterator;
    iterator->sequence = new Node*[10];

    int offset = 0;
    buildSequence(tree->root, iterator->sequence, offset);
    iterator->length = offset;

    iterator->position = ascending ? 0 : offset - 1;
    iterator->ascending = ascending;

    return iterator;
}

void deleteIterator(BinarySearchTreeIterator *&iterator)
{
    delete[] iterator->sequence;
    delete iterator;
    iterator = nullptr;
}

bool move(BinarySearchTreeIterator *iterator)
{
    if (iterator->ascending)
    {
        if (iterator->position < iterator->length - 1)
        {
            iterator->position++;
            return true;
        }
    }
    else
    {
        if (iterator->position > 0)
        {
            iterator->position--;
            return true;
        }
    }

    return false;
}

int getValue(BinarySearchTreeIterator *iterator)
{
    return iterator->sequence[iterator->position]->value;
}
