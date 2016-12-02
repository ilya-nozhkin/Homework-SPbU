#include "AstarSearch.h"
#include "PriorityQueue.h"

#include <stdlib.h>
#include <limits.h>

Point makeStartPoint()
{
    return makePoint(-2, -2);
}

bool isStartPoint(Point point)
{
    return point.x == -2 && point.y == -2;
}

struct Node
{
    Point parent;
    int length;
    int heuristic;
    bool passable;
    bool processed;
    bool queued;
};

struct AstarSearchEngine
{
    PriorityQueue *queue;
    Node **nodes;
    int width;
    int height;

    Point destination;
};

void initializeNode(Node &node)
{
    node.heuristic = -1;
    node.length = -1;
    node.parent = makePoint(-1, -1);
    node.passable = true;
    node.processed = false;
    node.queued = false;
}

AstarSearchEngine *createAstarSearchEngine(char **map, int width, int height)
{
    AstarSearchEngine *engine = new AstarSearchEngine;

    engine->nodes = new Node*[width];
    for (int i = 0; i < width; i++)
    {
        engine->nodes[i] = new Node[height];
        for (int j = 0; j < height; j++)
        {
            initializeNode(engine->nodes[i][j]);
            engine->nodes[i][j].passable = map[i][j] == '0';
        }
    }

    engine->width = width;
    engine->height = height;

    engine->queue = createPriorityQueue(width * height);

    engine->destination = makePoint(0, 0);

    return engine;
}

void deleteEngine(AstarSearchEngine *&engine)
{
    deleteQueue(engine->queue);

    for (int i = 0; i < engine->width; i++)
        delete[] engine->nodes[i];
    delete[] engine->nodes;

    delete engine;
    engine = nullptr;
}

int distance(Point begin, Point end)
{
    return abs(begin.x - end.x) + abs(begin.y - end.y);
}

int heuristic(Point current, Point destination)
{
    return distance(current, destination);
}

void start(AstarSearchEngine *engine, Point begin, Point end)
{
    engine->destination = end;

    Node &node = engine->nodes[begin.x][begin.y];
    node.heuristic = heuristic(begin, end);
    node.length = 0;
    node.parent = makeStartPoint();

    enqueue(engine->queue, node.heuristic, begin);
}

void tryToAddPoint(AstarSearchEngine *engine, Point point, Point previous)
{
    if (point.x < 0 || point.x >= engine->width ||
        point.y < 0 || point.y >= engine->height)
        return;

    Node &node = engine->nodes[point.x][point.y];
    Node &parent = engine->nodes[previous.x][previous.y];

    if (node.processed || !node.passable)
        return;

    int length = parent.length + distance(point, previous);
    int prediction = heuristic(point, engine->destination);
    int cost = length + prediction;

    if (node.queued)
    {
        if (length < node.length)
        {
            remove(engine->queue, point);

            node.length = length;
            node.parent = previous;

            enqueue(engine->queue, cost, point);
        }
    }
    else
    {
        node.length = length;
        node.parent = previous;
        node.queued = true;

        enqueue(engine->queue, cost, point);
    }
}

Point step(AstarSearchEngine *engine)
{
    Point point = dequeue(engine->queue);
    if (point.x == INT_MIN && point.y == INT_MIN)
        return point;

    Node &node = engine->nodes[point.x][point.y];

    node.queued = false;

    tryToAddPoint(engine, makePoint(point.x - 1, point.y),     point);
    tryToAddPoint(engine, makePoint(point.x,     point.y - 1), point);
    tryToAddPoint(engine, makePoint(point.x + 1, point.y),     point);
    tryToAddPoint(engine, makePoint(point.x,     point.y + 1), point);

    node.processed = true;

    return point;
}

bool stepError(Point answer)
{
    return answer.x == INT_MIN && answer.y == INT_MIN;
}

bool isValid(Point point)
{
    return point.x >= 0 && point.y >= 0;
}

Path *getPath(AstarSearchEngine *engine, Point point)
{
    Path *path = new Path;

    path->points = new Point[engine->width * engine->height];
    path->pointsCount = 0;

    Point cursor = point;
    while (isValid(engine->nodes[cursor.x][cursor.y].parent))
    {
        Point parent = engine->nodes[cursor.x][cursor.y].parent;

        path->points[path->pointsCount] = cursor;
        path->pointsCount++;

        cursor = parent;
    }

    path->points[path->pointsCount] = cursor;
    path->pointsCount++;

    return path;
}

void deletePath(Path *&path)
{
    delete[] path->points;
    delete path;
    path = nullptr;
}
