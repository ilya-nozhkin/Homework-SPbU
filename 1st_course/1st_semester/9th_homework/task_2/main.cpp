#include <iostream>
#include <fstream>
#include <limits.h>
#include <cstring>
#include "PriorityQueue.h"
#include "AstarSearch.h"

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

Point inputPoint(const char *what)
{
    Point point = makePoint(0, 0);
    cout << "Enter " << what << ": ";
    cin >> point.x >> point.y;
    return point;
}

AstarSearchEngine *prepareData(ifstream &file, char **buffer, int bufferSize, int &width, int &height)
{
    int cursor = 0;
    while(!file.eof())
    {
        file.getline(buffer[cursor], bufferSize);
        if (strlen(buffer[cursor]) == strlen(buffer[0]))
            cursor++;
    }

    width = strlen(buffer[0]);
    height = cursor;
    AstarSearchEngine *engine = createAstarSearchEngine(buffer, width, height);

    return engine;
}

bool computePath(AstarSearchEngine *engine, Point startPoint, Point destination, char **map)
{
    start(engine, startPoint, destination);

    bool success = false;
    bool process = true;
    while (process)
    {
        Point status = step(engine);

        if (stepError(status))
        {
            process = false;
            success = false;
        }
        else
        {
            map[status.x][status.y] = '-';

            if (equals(status, destination))
            {
                process = false;
                success = true;
            }
        }
    }

    return success;
}

void printResult(AstarSearchEngine *engine, char **map, int height,
                 bool found, Point startPoint, Point destination)
{
    if (found)
    {
        Path *path = getPath(engine, destination);
        for (int i = 0; i < path->pointsCount; i++)
        {
            Point point = path->points[i];
            map[point.x][point.y] = '*';
        }
        deletePath(path);
    }

    map[startPoint.x][startPoint.y] = 'S';
    map[destination.x][destination.y] = 'D';

    cout << endl;

    for (int j = 0; j < height; j++)
        cout << map[j] << endl;

    cout << endl;
    cout << "0 - passable point" << endl;
    cout << "1 - impassable point" << endl;
    cout << "S - start point" << endl;
    cout << "D - destination point" << endl;
    cout << "* - path" << endl;
    cout << "- - checked point" << endl;
}

void transponse(Point &point)
{
    int stored = point.x;
    point.x = point.y;
    point.y = stored;
}

char **createMap(int mapSize)
{
    char **map = new char*[mapSize];
    for (int i = 0; i < mapSize; i++)
    {
        map[i] = new char[mapSize];
        map[i][0] = '\0';
    }
    return map;
}

void deleteMap(char **&map, int mapSize)
{
    for (int i = 0; i < mapSize; i++)
        delete[] map[i];
    delete[] map;
    map = nullptr;
}

int main()
{
    const int fileNameSize = 256;
    char fileName[fileNameSize] = {'\0'};
    inputString(fileName, "the name of file");
    ifstream file(fileName);

    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return 1;
    }

    const int mapSize = 1024;
    char **map = createMap(mapSize);

    int width = 0;
    int height = 0;

    AstarSearchEngine *engine = prepareData(file, map, mapSize, width, height);

    file.close();

    Point startPoint =  inputPoint("the start point (x y)");
    Point destination = inputPoint("the destination point (x y)");

    transponse(startPoint);
    transponse(destination);

    bool found = computePath(engine, startPoint, destination, map);

    printResult(engine, map, height, found, startPoint, destination);

    deleteEngine(engine);
    deleteMap(map, mapSize);

    return 0;
}
