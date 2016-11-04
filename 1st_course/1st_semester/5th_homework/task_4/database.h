#pragma once

#include <istream>
#include <ostream>

struct Database;

Database *createDatabase();
void deleteDatabase(Database *&database);
void clearDatabase(Database *database);

int addRecord(Database *database, const char *name, const char *data);
void removeRecord(Database *database, int index);

int findRecordByName(Database *database, const char *name);
int findRecordByData(Database *database, const char *data);

bool getNameByIndex(Database *database, int index, char *buffer);
bool getDataByIndex(Database *database, int index, char *buffer);

void serialize(Database *database, std::ostream &stream);
void deserialize(Database *database, std::istream &stream);
