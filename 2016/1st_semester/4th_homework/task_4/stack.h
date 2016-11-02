#pragma once

struct Stack;

Stack *createStack();
void deleteStack(Stack *&stack);
bool isEmpty(Stack *stack);
void push(Stack *stack, int value);
bool pop(Stack *stack);
int top(Stack *stack);
