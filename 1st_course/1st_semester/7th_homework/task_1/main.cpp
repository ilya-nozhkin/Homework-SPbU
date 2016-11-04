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

int main()
{
    printHelp();

    Set *set = createSet();

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
