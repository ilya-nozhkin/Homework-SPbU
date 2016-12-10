#pragma once

struct Point
{
    int x;
    int y;
};

Point makePoint(int x, int y);
bool equals(Point left, Point right);

struct PriorityQueue;

PriorityQueue *createPriorityQueue(int maxSize);
void deleteQueue(PriorityQueue *&queue);

void  clear(PriorityQueue *queue);
bool  enqueue(PriorityQueue *queue, int key, Point value);
Point dequeue(PriorityQueue *queue);
bool  exists(PriorityQueue *queue, Point value);
bool  remove(PriorityQueue *queue, Point value);

bool isEmpty(PriorityQueue *queue);
