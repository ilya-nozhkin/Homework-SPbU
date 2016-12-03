#pragma once

struct DijkstraSearchEngine;

DijkstraSearchEngine *createDijkstraSearchEngine(int nodes);
void deleteEngine(DijkstraSearchEngine *&engine);

void addLink(DijkstraSearchEngine *engine, int left, int right, int weight);
void startSearch(DijkstraSearchEngine *engine, int startNode);

int step(DijkstraSearchEngine *engine);

struct Path
{
    int *nodes;
    int nodeCount;
    int length;
};

Path *getPath(DijkstraSearchEngine *engine, int node);
void deletePath(Path *&path);
