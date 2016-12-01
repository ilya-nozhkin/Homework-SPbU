#include <iostream>
#include <fstream>
#include <limits.h>
#include "PriorityQueue.h"
#include "DijkstraSearch.h"

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

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

DijkstraSearchEngine *prepareData(ifstream &file, int &cities)
{
    int roads = 0;

    file >> cities >> roads;

    DijkstraSearchEngine *engine = createDijkstraSearchEngine(cities);

    for (int i = 0; i < roads; i++)
    {
        int left = 0;
        int right = 0;
        int length = 0;
        file >> left >> right >> length;
        addLink(engine, left, right, length);
    }

    return engine;
}

void findPathes(DijkstraSearchEngine *engine, int *order, int &count)
{
    bool process = true;
    while (process)
    {
        int status = step(engine);
        if (status == INT_MIN)
            process = false;
        else
        {
            order[count] = status;
            count++;
        }
    }
}

void printResult(DijkstraSearchEngine *engine, int *order, int count)
{
    for (int i = 0; i < count; i++)
    {
        Path *path = getPath(engine, order[i]);

        cout << "step-" << i << " city-" << order[i] << " distance-" << path->length << " path-";

        for (int j = 0; j < path->nodeCount; j++)
            cout << path->nodes[j] << (j < path->nodeCount - 1 ? "," : "");

        cout << endl;

        deletePath(path);
    }
}

int main()
{
    if (!priorityQueueTest())
    {
        cout << "Internal error: priority queue test has failed" << endl;
        return 1;
    }

    const int fileNameSize = 256;
    char fileName[fileNameSize] = {'\0'};
    inputString(fileName, "the name of file");
    ifstream file(fileName);

    int cities = 0;
    DijkstraSearchEngine *engine = prepareData(file, cities);

    startSearch(engine, 0);

    int *order = new int[cities];
    int count = 0;

    findPathes(engine, order, count);
    printResult(engine, order, count);

    delete[] order;
    deleteEngine(engine);

    return 0;
}
