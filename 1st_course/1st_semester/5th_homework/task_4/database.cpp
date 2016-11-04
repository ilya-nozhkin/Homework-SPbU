#include "database.h"

#include <cstring>

using namespace std;

struct Record
{
    char *name;
    char *data;

    Record *previous;
    Record *next;

    int index;
};

Record *createRecord(const char *name, const char *data, Record *previous, Record *next, int index)
{
    Record *record = new Record;

    record->name = new char[strlen(name) + 1];
    strcpy(record->name, name);

    record->data = new char[strlen(data) + 1];
    strcpy(record->data, data);

    record->previous = previous;
    record->next = next;
    record->index = index;

    return record;
}

void deleteRecord(Record *&record)
{
    delete[] record->name;
    delete[] record->data;

    delete record;
    record = nullptr;
}

struct Database
{
    Record *head;
    int size;

    Record *cursor;
};

Database *createDatabase()
{
    Database *database = new Database;

    database->head = nullptr;
    database->cursor = database->head;
    database->size = 0;

    return database;
}

void deleteDatabase(Database *&database)
{
    clearDatabase(database);

    delete database;
    database = nullptr;
}

void clearDatabase(Database *database)
{
    while (database->head != nullptr)
    {
        Record *toDelete = database->head;
        database->head = database->head->next;
        deleteRecord(toDelete);
    }

    database->cursor = database->head;
    database->size = 0;
}

int addRecord(Database *database, const char *name, const char *data)
{
    const int firstIndex = 0;
    if (database->size == 0)
    {
        database->head = createRecord(name, data, nullptr, nullptr, firstIndex);
        database->cursor = database->head;
        database->size++;
        return firstIndex;
    }

    Record *&cursor = database->cursor;
    cursor = database->head;

    int newIndex = firstIndex + 1;
    while (cursor->next != nullptr && cursor->next->index == newIndex)
    {
        cursor = cursor->next;
        newIndex++;
    }

    Record *newRecord = createRecord(name, data, cursor, cursor->next, newIndex);

    if (cursor->next != nullptr)
        cursor->next->previous = newRecord;
    cursor->next = newRecord;
    database->size++;

    return newIndex;
}

bool moveCursorToIndex(Database *database, int index)
{
    Record *&cursor = database->cursor;

    while (cursor != nullptr && index < cursor->index)
        cursor = cursor->previous;

    while (cursor != nullptr && index > cursor->index)
        cursor = cursor->next;

    if (cursor == nullptr)
        cursor = database->head;

    return index == cursor->index;
}

void removeRecord(Database *database, int index)
{
    if (moveCursorToIndex(database, index))
    {
        Record *toDelete = database->cursor;

        database->cursor = toDelete->previous != nullptr ?
                           toDelete->previous : toDelete->next;

        if (toDelete->next != nullptr)
            toDelete->next->previous = toDelete->previous;

        if (toDelete->previous != nullptr)
            toDelete->previous->next = toDelete->next;

        deleteRecord(toDelete);
        database->size--;
    }
}

int findRecordByName(Database *database, const char *name)
{
    Record *&cursor = database->cursor;
    cursor = database->head;

    while (cursor != nullptr && strcmp(cursor->name, name) != 0)
        cursor = cursor->next;

    if (cursor != nullptr)
        return cursor->index;
    else
        cursor = database->head;

    return -1;
}

int findRecordByData(Database *database, const char *data)
{
    Record *&cursor = database->cursor;
    cursor = database->head;

    while (cursor != nullptr && strcmp(cursor->data, data) != 0)
        cursor = cursor->next;

    if (cursor != nullptr)
        return cursor->index;
    else
        cursor = database->head;

    return -1;
}

bool getNameByIndex(Database *database, int index, char *buffer)
{
    if (moveCursorToIndex(database, index))
    {
        strcpy(buffer, database->cursor->name);

        return true;
    }

    return false;
}

bool getDataByIndex(Database *database, int index, char *buffer)
{
    if (moveCursorToIndex(database, index))
    {
        strcpy(buffer, database->cursor->data);

        return true;
    }

    return false;
}

void serialize(Database *database, std::ostream &stream)
{
    Record *&cursor = database->cursor;
    cursor = database->head;

    stream << database->size << endl;

    while (cursor != nullptr)
    {
        stream << cursor->index << ' ' << cursor->name << ' ' << cursor->data << endl;

        cursor = cursor->next;
    }

    cursor = database->head;
}

void deserialize(Database *database, std::istream &stream)
{
    clearDatabase(database);

    int size = 0;
    stream >> size;

    for (int i = 0; i < size; i++)
    {
        const int nameSize = 128;
        const int dataSize = 512;
        char name[nameSize] = {'\0'};
        char data[dataSize] = {'\0'};
        int index = 0;

        stream >> index;
        stream >> name;
        stream >> data;

        Record *newRecord = createRecord(name, data, database->cursor, nullptr, index);

        if (i == 0)
        {
            database->head = newRecord;
            database->cursor = database->head;
        }
        else
        {
            database->cursor->next = newRecord;
            database->cursor = database->cursor->next;
        }

        database->size++;
    }
}
