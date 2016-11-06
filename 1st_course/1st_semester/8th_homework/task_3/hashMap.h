#pragma once

#include "myString.h"
#include <inttypes.h>

struct HashMap;

typedef uint64_t HashFunction(const char *source);

HashFunction hashDJB2;
HashFunction hashSDBM;
HashFunction hashXor;
HashFunction hashSum;

HashMap *createHashMap(HashFunction *hash);
void deleteHashMap(HashMap *&map);

bool add(HashMap *map, String *key, int value);
bool remove(HashMap *map, String *key);
int  getValue(HashMap *map, String *key);
bool setValue(HashMap *map, String *key, int value);

bool add(HashMap *map, const char *key, int value);
bool remove(HashMap *map, const char *key);
int  getValue(HashMap *map, const char *key);
bool setValue(HashMap *map, const char *key, int value);

int   size(HashMap *map);
float loadFactor(HashMap *map);
float averageChainLength(HashMap *map);
int   maximumChainLength(HashMap *map);
int   emptyChainsNumber(HashMap *map);

void getLongestChain(HashMap *map, String **keys, int *values);
