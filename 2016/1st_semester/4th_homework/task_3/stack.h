#pragma once

struct Stack;

Stack *createStack();
void deleteStack(Stack *&stack);
bool isEmpty(Stack *stack);
void push(Stack *stack, char value);
bool pop(Stack *stack);
char top(Stack *stack);
