#include <iostream>
#include <fstream>

#include "database.h"

using namespace std;

enum Command
{
    exitCommandId = 0,
    addCommandId = 1,
    findByNameCommandId = 2,
    findByNumberCommandId = 3,
    saveCommandId = 4
};

const char *getDatabaseFilename()
{
    return "numbers.db";
}

void printHelp()
{
    cout << "This program provides interface to database with telephone numbers" << endl;
    cout << "supported commands:" << endl;
    cout << exitCommandId << " - exit" << endl;
    cout << addCommandId << " - add record" << endl;
    cout << findByNameCommandId << " - find number by name" << endl;
    cout << findByNumberCommandId << " - find name by number" << endl;
    cout << saveCommandId << " - save current database to file (" << getDatabaseFilename() << ")" << endl;
}

void addRecordCommand(Database *database)
{
    cout << "Enter name (without spaces) and number separated by space: " << endl;

    const int nameSize = 128;
    const int dataSize = 512;

    char name[nameSize] = {'\0'};
    char data[dataSize] = {'\0'};

    cin >> name;
    cin >> data;

    int index = addRecord(database, name, data);

    cout << "record has been added with index " << index << endl;
}

void findNumberByNameCommand(Database *database)
{
    cout << "Enter name (without spaces): ";

    const int nameSize = 128;
    char name[nameSize] = {'\0'};
    cin >> name;

    int index = findRecordByName(database, name);

    if (index >= 0)
    {
        const int dataSize = 512;
        char data[dataSize] = {'\0'};
        getDataByIndex(database, index, data);
        cout << "number: " << data << endl;
    }
    else
    {
        cout << "The database doesn't contain record with name \"" << name << "\"" << endl;
    }
}

void findNameByNumberCommand(Database *database)
{
    cout << "Enter number: ";

    const int numberSize = 512;
    char number[numberSize] = {'\0'};
    cin >> number;

    int index = findRecordByData(database, number);

    if (index >= 0)
    {
        const int nameSize = 512;
        char name[nameSize] = {'\0'};
        getNameByIndex(database, index, name);
        cout << "name: " << name << endl;
    }
    else
    {
        cout << "The database doesn't contain record with number \"" << number << "\"" << endl;
    }
}

void saveCommand(Database *database)
{
    ofstream file(getDatabaseFilename());

    serialize(database, file);

    file.close();

    cout << "The database has been saved to file \"" << getDatabaseFilename() << "\"" << endl;
 }

void loadDatabase(Database *database)
{
    ifstream file(getDatabaseFilename());

    if (file.is_open())
    {
        deserialize(database, file);

        file.close();
    }
}

int main()
{
    printHelp();

    Database *database = createDatabase();

    loadDatabase(database);

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
                addRecordCommand(database);
                break;
            }
            case findByNameCommandId:
            {
                findNumberByNameCommand(database);
                break;
            }
            case findByNumberCommandId:
            {
                findNameByNumberCommand(database);
                break;
            }
            case saveCommandId:
            {
                saveCommand(database);
                break;
            }
            default:
            {
                cout << "unknown command" << endl;
            }
        }
    }

    deleteDatabase(database);

    return 0;
}
