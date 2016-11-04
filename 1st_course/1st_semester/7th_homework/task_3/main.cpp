#include <iostream>

#include "Set.h"

using namespace std;

enum Command
{
    exitCommandId = 0,
    addCommandId = 1,
    removeCommandId = 2,
    existsCommandId = 3,
    printAscendingCommandId = 4,
    printDescendingCommandId = 5,
    printTreeCommandId = 6
};

void printHelp()
{
    cout << "This program provides you an interface to set" << endl;
    cout << "Supported commands:" << endl;
    cout << exitCommandId << " - exit" << endl;
    cout << addCommandId << " - add value" << endl;
    cout << removeCommandId << " - remove value" << endl;
    cout << existsCommandId << " - check if value exists in set" << endl;
    cout << printAscendingCommandId << " - print set ascending" << endl;
    cout << printDescendingCommandId << " - print set descending" << endl;
    cout << printTreeCommandId << " - print set as tree" << endl;
}

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

void addCommand(Set *set)
{
    int value = inputNumber("a value which you want to add");

    bool status = add(set, value);
    if (status)
        cout << "value has been added successfully" << endl;
    else
        cout << "value already exists in set" << endl;
}

void removeCommand(Set *set)
{
    int value = inputNumber("a value which you want to remove");

    bool status = remove(set, value);
    if (status)
        cout << "value has been removed successfully" << endl;
    else
        cout << "set doesn't contain this value" << endl;
}

void existsCommand(Set *set)
{
    int value = inputNumber("a value which you want to find");

    bool status = exists(set, value);
    if (status)
        cout << "set contains this value" << endl;
    else
        cout << "set doesn't contain this value" << endl;
}

void printAscendingCommand(Set *set)
{
    printAscending(set, cout);
    cout << endl;
}

void printDescendingCommand(Set *set)
{
    printDescending(set, cout);
    cout << endl;
}

void printTreeCommand(Set *set)
{
    serialize(set, cout);
    cout << endl;
}

bool addToSetTest(Set *set, int *values, int &size, int valuesNumber)
{
    const int maxValue = 200;

    for (int i = 0; i < valuesNumber; i++)
    {
        int value = rand() % (maxValue + 1);

        if (add(set, value))
        {
            values[size] = value;
            size++;
        }

        if (!isBalanced(set))
            return false;
    }

    for (int i = 0; i < size; i++)
    {
        if (!exists(set, values[i]))
        {
            return false;
        }
    }

    return true;
}

int find(int *values, int size, int value)
{
    for (int i = 0; i < size; i++)
    {
        if (values[i] == value)
        {
            return i;
        }
    }

    return -1;
}

void remove(int *values, int &size, int position)
{
    for (int i = position; i < size - 1; i++)
        values[i] = values[i + 1];
    size--;
}

bool removeFromSetTest(Set *set, int *values, int &size, int valuesNumber)
{
    const int maxValue = 180;

    for (int i = 0; i < valuesNumber; i++)
    {
        int value = rand() % (maxValue + 1);

        int position = find(values, size, value);

        if (position >= 0)
        {
            remove(values, size, position);
            remove(set, value);

            bool balanced = isBalanced(set);
            bool valueExists = exists(set, value);
            if (valueExists || !balanced)
                return false;
        }
    }

    for (int i = 0; i < size; i++)
    {
        if (!exists(set, values[i]))
        {
            return false;
        }
    }

    return true;
}

bool doAutomaticTest(Set *set)
{
    const int valuesNumber = 150;
    int values[valuesNumber] = {0};
    int size = 0;

    bool status = addToSetTest(set, values, size, valuesNumber);

    if (status)
    {
        status = removeFromSetTest(set, values, size, valuesNumber);
    }

    clear(set);
    return status;
}

bool doAutomaticTests(Set *set, int tests)
{
    for (int i = 0; i < tests; i++)
        if (!doAutomaticTest(set))
            return false;

    return true;
}

int main()
{
    srand(time(nullptr));

    Set *set = createSet();

    const int tests = 500;
    if (doAutomaticTests(set, tests))
    {
        cout << tests << " tests has performed successfully" << endl << endl;
    }
    else
    {
        cout << "automatic tests have failed" << endl;
        return 1;
    }

    printHelp();

    bool process = true;
    while (process)
    {
        Command command = static_cast<Command>(inputNumber("the command"));

        switch (command)
        {
            case exitCommandId:
            {
                process = false;
                break;
            }
            case addCommandId:
            {
                addCommand(set);
                break;
            }
            case removeCommandId:
            {
                removeCommand(set);
                break;
            }
            case existsCommandId:
            {
                existsCommand(set);
                break;
            }
            case printAscendingCommandId:
            {
                printAscendingCommand(set);
                break;
            }
            case printDescendingCommandId:
            {
                printDescendingCommand(set);
                break;
            }
            case printTreeCommandId:
            {
                printTreeCommand(set);
                break;
            }
            default:
            {
                cout << "unknown command" << endl;
            }
        }
    }

    deleteSet(set);

    return 0;
}
