#include "CalculatingTree.h"

#include <limits.h>

using namespace std;

struct Node
{
    char type;
    int value;
    Node *left;
    Node *right;
};

struct CalculatingTree
{
    Node *root;
};

CalculatingTree *createCalculatingTree()
{
    CalculatingTree *tree = new CalculatingTree;
    tree->root = nullptr;
    return tree;
}

Node *createNode(char type, int value)
{
    Node *node = new Node;
    node->type = type;
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

void deleteTree(CalculatingTree *&tree)
{
    deleteNode(tree->root);

    delete tree;
    tree = nullptr;
}

int calculate(Node *node)
{
    if (node->type == 0)
        return node->value;

    int left = calculate(node->left);
    int right = calculate(node->right);

    if (node->type == '+')
        return left + right;

    if (node->type == '-')
        return left - right;

    if (node->type == '*')
        return left * right;

    if (node->type == '/')
        return left / right;

    return INT_MIN;
}

int calculate(CalculatingTree *tree)
{
    return calculate(tree->root);
}

char parse(Node *&node, std::istream &stream, char previous);

char parseOperator(Node *&node, std::istream &stream, char current)
{
    stream.get(current);
    node = createNode(current, 0);

    stream.get(current);

    while (current == ' ')
        stream.get(current);
    current = parse(node->left, stream, current);

    while (current == ' ')
        stream.get(current);
    current = parse(node->right, stream, current);

    stream.get(current);

    return current;
}

char parseNumber(Node *&node, std::istream &stream, char current)
{
    bool positive = true;

    if (current == '-')
    {
        positive = false;
        stream.get(current);
    }

    int value = 0;

    while (current >= '0' && current <= '9')
    {
        value *= 10;
        value += current - '0';

        stream.get(current);
    }

    if (!positive)
        value = -value;

    node = createNode(0, value);

    return current;
}

char parse(Node *&node, std::istream &stream, char previous)
{
    char current = previous;

    if (current == '(')
    {
        current = parseOperator(node, stream, current);
    }
    else
    {
        current = parseNumber(node, stream, current);
    }

    return current;
}

void parse(CalculatingTree *tree, std::istream &stream)
{
    char current = 0;
    stream.get(current);
    parse(tree->root, stream, current);
}

void printInfixForm(Node *node, std::ostream &stream)
{
    if (node->type == 0)
    {
        stream << node->value;
        return;
    }

    stream << '(';

    printInfixForm(node->left, stream);

    stream << ' ' << node->type << ' ';

    printInfixForm(node->right, stream);

    stream << ')';
}

void printInfixForm(CalculatingTree *tree, std::ostream &stream)
{
    printInfixForm(tree->root, stream);
}
