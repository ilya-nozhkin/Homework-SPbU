#include <iostream>
#include "list.h"

using namespace std;

enum Command
{
    exitCommandId = 0,
    addCommandId = 1,
    removeCommandId = 2,
    printCommandId = 3
};

void printList(List *list)
{
    for (int i = 0; i < size(list); i++)
        cout << get(list, i) << ' ';
    cout << endl;
}

void printHelp()
{
    cout << "This program provides you an interface to linked list" << endl;
    cout << "Supported commands: " << endl;
    cout << exitCommandId << " - exit;" << endl;
    cout << addCommandId << " - add a value to sorted list;" << endl;
    cout << removeCommandId << " - remove a value from sorted list;" << endl;
    cout << printCommandId << " - print list." << endl;
}

void commandAdd(List *list)
{
    int value = 0;

    cout << "enter the value which you want to add: ";
    cin >> value;

    add(list, value);

    cout << "value " << value << " has been added sucessfully" << endl;
}

void commandRemove(List *list)
{
    int value = 0;

    cout << "enter the value which you want to remove: ";
    cin >> value;

    bool status = remove(list, value);

    if (status)
        cout << "value " << value << " has been removed successfully" << endl;
    else
        cout << "list doesn't contain value " << value << endl;
}

void commandPrint(List *list)
{
    cout << "list contains " << size(list) << " elements: " << endl;
    printList(list);
}

int main()
{
    printHelp();

    List *list = createList();

    bool process = true;
    while (process)
    {
        int commandId = 0;
        cout << "Enter the command: ";
        cin >> commandId;

        Command command = static_cast<Command>(commandId);

        switch(command)
        {
            case exitCommandId:
            {
                process = false;
                break;
            }
            case addCommandId:
            {
                commandAdd(list);
                break;
            }
            case removeCommandId:
            {
                commandRemove(list);
                break;
            }
            case printCommandId:
            {
                commandPrint(list);
                break;
            }
            default:
            {
                cout << "unknown command" << endl;
            }
        }
    }

    deleteList(list);

    return 0;
}
