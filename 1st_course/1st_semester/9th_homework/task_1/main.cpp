#include <iostream>
#include "PriorityQueue.h"

using namespace std;

bool enqueueTestStep1(PriorityQueue *queue, int values)
{
    bool status = true;
    for (int i = 0; i < values; i++)
    {
        int key = values * 2 - (i % 2 == 1 ? i : i + values);
        if (!enqueue(queue, key, i))
            status = false;
    }
    return status;
}

bool dequeueTestStep1(PriorityQueue *queue, int values)
{
    bool status = true;
    for (int i = 0; i < values / 2; i++)
    {
        int value = dequeue(queue);
        if (value != (values - 2 - i * 2))
            status = false;
    }
    return status;
}

bool enqueueTestStep2(PriorityQueue *queue, int values)
{
    const int value = 100;
    bool status = true;
    for (int i = 0; i < values / 4; i++)
    {
        if (!enqueue(queue, values * 4, value))
            status = false;
    }
    return status;
}

bool dequeueTestStep2(PriorityQueue *queue, int values)
{
    bool status = true;
    for (int i = 0; i < values / 2; i++)
    {
        int value = dequeue(queue);
        if (value != (values - 1 - i * 2))
            status = false;
    }
    return status;
}

bool priorityQueueTest()
{
    const int values = 128;

    bool status = true;
    PriorityQueue *queue = createPriorityQueue(values);

    if (status)
        status = enqueueTestStep1(queue, values);

    if (status)
        status = dequeueTestStep1(queue, values);

    if (status)
        status = enqueueTestStep2(queue, values);

    if (status)
        status = dequeueTestStep2(queue, values);

    deleteQueue(queue);

    return status;
}

int main()
{
    if (!priorityQueueTest())
    {
        cout << "Internal error: priority queue test has failed" << endl;
        return 1;
    }



    return 0;
}
