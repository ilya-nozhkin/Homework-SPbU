#pragma once

struct PriorityQueue;

PriorityQueue *createPriorityQueue(int maxSize);
void deleteQueue(PriorityQueue *&queue);

void clear(PriorityQueue *queue);
bool enqueue(PriorityQueue *queue, int key, int value);
int  dequeue(PriorityQueue *queue);

bool isEmpty(PriorityQueue *queue);
