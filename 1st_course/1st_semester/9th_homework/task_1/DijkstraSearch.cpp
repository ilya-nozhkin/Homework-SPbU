#include "DijkstraSearch.h"
#include "PriorityQueue.h"
#include <limits.h>

struct Node
{
    int parent;
    int length;
    int *links;
    int *weights;
    int linkCount;
    bool processed;
};

struct DijkstraSearchEngine
{
    Node *nodes;
    int nodeCount;
    PriorityQueue *queue;
};

void initializeNode(Node &node, int maxLinks)
{
    node.parent = -1;
    node.length = -1;
    node.linkCount = 0;
    node.links = new int[maxLinks];
    node.weights = new int[maxLinks];
    node.processed = false;
}

void terminateNode(Node &node)
{
    delete[] node.links;
    delete[] node.weights;
}

DijkstraSearchEngine *createDijkstraSearchEngine(int nodes)
{
    DijkstraSearchEngine *engine = new DijkstraSearchEngine;
    engine->queue = createPriorityQueue(nodes);
    engine->nodeCount = nodes;
    engine->nodes = new Node[nodes];

    for (int i = 0; i < nodes; i++)
        initializeNode(engine->nodes[i], nodes);

    return engine;
}

void deleteEngine(DijkstraSearchEngine *&engine)
{
    deleteQueue(engine->queue);
    for (int i = 0; i < engine->nodeCount; i++)
        terminateNode(engine->nodes[i]);
    delete[] engine->nodes;
    delete engine;

    engine = nullptr;
}

void addLink(Node &node, int link, int weight)
{
    node.links[node.linkCount] = link;
    node.weights[node.linkCount] = weight;
    node.linkCount++;
}

void addLink(DijkstraSearchEngine *engine, int left, int right, int weight)
{
    addLink(engine->nodes[left], right, weight);
    addLink(engine->nodes[right], left, weight);
}

void startSearch(DijkstraSearchEngine *engine, int startNode)
{
    engine->nodes[startNode].parent = -2;
    engine->nodes[startNode].length = 0;
    enqueue(engine->queue, 0, startNode);
}

int step(DijkstraSearchEngine *engine)
{
    int index = dequeue(engine->queue);
    if (index == INT_MIN)
        return index;

    Node &node = engine->nodes[index];

    for (int i = 0; i < node.linkCount; i++)
    {
        int link = node.links[i];
        Node &next = engine->nodes[link];

        if (!next.processed)
        {
            int length = node.length + node.weights[i];

            if (!exists(engine->queue, link))
            {
                next.length = length;
                next.parent = index;

                enqueue(engine->queue, length, link);
            }
        }
    }

    node.processed = true;

    return index;
}

int findWeight(Node &node, int destination)
{
    for (int i = 0; i < node.linkCount; i++)
        if (node.links[i] == destination)
            return node.weights[i];
    return INT_MIN;
}

Path *getPath(DijkstraSearchEngine *engine, int node)
{
    Path *path = new Path;
    path->nodes = new int[engine->nodeCount];
    path->nodeCount = 0;
    path->length = 0;

    int cursor = node;
    while (engine->nodes[cursor].parent >= 0)
    {
        int parent = engine->nodes[cursor].parent;

        path->length += findWeight(engine->nodes[cursor], parent);
        path->nodes[path->nodeCount] = cursor;
        path->nodeCount++;

        cursor = parent;
    }

    path->nodes[path->nodeCount] = cursor;
    path->nodeCount++;

    return path;
}

void deletePath(Path *&path)
{
    delete[] path->nodes;
    delete path;
    path = nullptr;
}
