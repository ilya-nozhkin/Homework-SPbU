#pragma once

struct PriorityQueue;

PriorityQueue *createPriorityQueue(int maxSize);
void deleteQueue(PriorityQueue *&queue);

void clear(PriorityQueue *queue);
bool enqueue(PriorityQueue *queue, int key, int value);
int  dequeue(PriorityQueue *queue);
bool exists(PriorityQueue *queue, int value);
bool remove(PriorityQueue *queue, int value);

bool isEmpty(PriorityQueue *queue);
