#include "AvlTree.h"

using namespace std;

struct Node
{
    int height;
    int value;
    Node *left;
    Node *right;
};

struct AvlTree
{
    Node *root;
    int size;
};

Node *createNode(int value)
{
    Node *node = new Node;
    node->height = 1;
    node->value = value;
    node->left = nullptr;
    node->right = nullptr;
    return node;
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

int height(Node *node)
{
    if (node == nullptr)
        return 0;

    return node->height;
}

void updateHeight(Node *node)
{
    int left = 0;
    if (node->left != nullptr)
        left = node->left->height;

    int right = 0;
    if (node->right != nullptr)
        right = node->right->height;

    node->height = (left > right ? left : right) + 1;
}

int balanceFactor(Node *node)
{
    return height(node->right) - height(node->left);
}

void rotateLeft(Node *&root)
{
    Node *stored = root;
    root = root->right;

    Node *center = root->left;
    root->left = stored;
    stored->right = center;

    updateHeight(stored);
    updateHeight(root);
}

void rotateRight(Node *&root)
{
    Node *stored = root;
    root = root->left;

    Node *center = root->right;
    root->right = stored;
    stored->left = center;

    updateHeight(stored);
    updateHeight(root);
}

void balance(Node *&node)
{
    if (balanceFactor(node) == 2)
    {
        if (balanceFactor(node->right) < 0)
            rotateRight(node->right);
        rotateLeft(node);
    }
    else if (balanceFactor(node) == -2)
    {
        if (balanceFactor(node->left) > 0)
            rotateLeft(node->left);
        rotateRight(node);
    }
}

AvlTree *createAvlTree()
{
    AvlTree *tree = new AvlTree;
    tree->root = nullptr;
    tree->size = 0;

    return tree;
}

void deleteTree(AvlTree *&tree)
{
    clear(tree);

    delete tree;
    tree = nullptr;
}

void clear(AvlTree *tree)
{
    if (tree->root != nullptr)
        deleteNode(tree->root);
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
        bool status = add(node->right, value);
        updateHeight(node);
        balance(node);
        return status;
    }
    else if (value < node->value)
    {
        bool status = add(node->left, value);
        updateHeight(node);
        balance(node);
        return status;
    }

    return false;
}

bool add(AvlTree *tree, int value)
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

void removeMaxLower(Node *root, Node *&current)
{
    if (current->right != nullptr)
    {
        removeMaxLower(root, current->right);
        updateHeight(current);
        balance(current);
    }
    else
    {
        root->value = current->value;
        tryToRemoveRightEmpty(current);
    }
}

void removeMinHigher(Node *root, Node *&current)
{
    if (current->left != nullptr)
    {
        removeMinHigher(root, current->left);
        updateHeight(current);
        balance(current);
    }
    else
    {
        root->value = current->value;
        tryToRemoveLeftEmpty(current);
    }
}

bool remove(Node *&node, int value)
{
    if (node == nullptr)
        return false;

    if (value > node->value)
    {
        bool status = remove(node->right, value);
        updateHeight(node);
        balance(node);
        return status;
    }
    else if (value < node->value)
    {
        bool status = remove(node->left, value);
        updateHeight(node);
        balance(node);
        return status;
    }

    if (node->left == nullptr && node->right == nullptr)
    {
        deleteNode(node);
    }
    else
    {
        if (tryToRemoveLeftEmpty(node))
            return true;

        if (tryToRemoveRightEmpty(node))
            return true;

        if (balanceFactor(node) > 0)
            removeMinHigher(node, node->right);
        else
            removeMaxLower(node, node->left);

        updateHeight(node);
        balance(node);
    }

    return true;
}

bool remove(AvlTree *tree, int value)
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

bool exists(AvlTree *tree, int value)
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

void serialize(AvlTree *tree, ostream &stream)
{
    serialize(tree->root, stream);
}

int size(AvlTree *tree)
{
    return tree->size;
}

bool isBalanced(Node *node)
{
    bool left = true;
    if (node->left != nullptr)
        left = isBalanced(node->left);

    bool right = true;
    if (node->right != nullptr)
        right = isBalanced(node->right);

    bool current = abs(balanceFactor(node)) <= 1;

    return left && right && current;
}

bool isBalanced(AvlTree *tree)
{
    if (tree->root == nullptr)
        return true;

    return isBalanced(tree->root);
}

struct AvlTreeIterator
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

AvlTreeIterator *createAvlTreeIterator(AvlTree *tree, bool ascending)
{
    if (tree->root == nullptr)
        return nullptr;

    AvlTreeIterator *iterator = new AvlTreeIterator;
    iterator->sequence = new Node*[10];

    int offset = 0;
    buildSequence(tree->root, iterator->sequence, offset);
    iterator->length = offset;

    iterator->position = ascending ? 0 : offset - 1;
    iterator->ascending = ascending;

    return iterator;
}

void deleteIterator(AvlTreeIterator *&iterator)
{
    delete[] iterator->sequence;
    delete iterator;
    iterator = nullptr;
}

bool move(AvlTreeIterator *iterator)
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

int getValue(AvlTreeIterator *iterator)
{
    return iterator->sequence[iterator->position]->value;
}
