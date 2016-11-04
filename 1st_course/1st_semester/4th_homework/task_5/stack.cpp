#include "stack.h"

struct StackElement
{
    int value;
    StackElement *next;
};

struct Stack
{
    StackElement *top;
};

Stack *createStack()
{
    Stack *stack = new Stack;
    stack->top = nullptr;
    return stack;
}

void deleteStack(Stack *&stack)
{
    while (!isEmpty(stack))
        pop(stack);

    delete stack;
}

bool isEmpty(Stack *stack)
{
    return stack->top == nullptr;
}

void push(Stack *stack, int value)
{
    StackElement *newElement = new StackElement;
    newElement->next = stack->top;
    newElement->value = value;
    stack->top = newElement;
}

bool pop(Stack *stack)
{
    if (isEmpty(stack))
        return false;

    StackElement *toDelete = stack->top;
    stack->top = stack->top->next;
    delete toDelete;

    return true;
}

int top(Stack *stack)
{
    if (!isEmpty(stack))
        return stack->top->value;

    return -1;
}
