#pragma once

#include "PriorityQueue.h"

struct AstarSearchEngine;

AstarSearchEngine *createAstarSearchEngine(char **map, int width, int height);
void deleteEngine(AstarSearchEngine *&engine);

void start(AstarSearchEngine *engine, Point begin, Point end);

Point step(AstarSearchEngine *engine);
bool stepError(Point answer);

struct Path
{
    Point *points;
    int pointsCount;
};

Path *getPath(AstarSearchEngine *engine, Point point);
void deletePath(Path *&path);
