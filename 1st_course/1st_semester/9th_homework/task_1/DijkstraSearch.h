#pragma once

struct DijkstraSearchEngine;

struct StepResult
{
    int node;
    int distance;
    int *path;
    int pathLength;
};

DijkstraSearchEngine *createDijkstraSearchEngine(int nodes);
void deleteEngine(DijkstraSearchEngine *&engine);

void addLink(DijkstraSearchEngine *engine, int left, int right, int weight);
void startSearch(DijkstraSearchEngine *engine, int startNode);

StepResult *step(DijkstraSearchEngine *engine);
void deleteStepResult(StepResult *&result);
