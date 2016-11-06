#include "hashMap.h"

#include <limits.h>

uint64_t hashDJB2(const char *source)
{
    uint64_t hash = 5381;
    unsigned int cursor = 0;

    while (source[cursor])
    {
        hash = ((hash << 5) + hash) + ((unsigned char) source[cursor]);
        cursor++;
    }

    return hash;
}

uint64_t hashSDBM(const char *source)
{
    uint64_t hash = 0;
    unsigned int cursor = 0;

    while (source[cursor])
    {
        hash = ((unsigned char) source[cursor]) + (hash << 6) + (hash << 16) - hash;
        cursor++;
    }

    return hash;
}

uint64_t hashXor(const char *source)
{
    uint64_t hash = 0;
    unsigned int cursor = 0;

    while (source[cursor])
    {
        hash = (hash << 6) ^ (hash >> 26) ^ ((unsigned char) source[cursor]);
        cursor++;
    }

    return hash;
}

uint64_t hashSum(const char *source)
{
    uint64_t hash = 0;
    unsigned int cursor = 0;

    while (source[cursor])
    {
        hash = hash + ((unsigned char) source[cursor]);
        cursor++;
    }

    return hash;
}

struct Node
{
    String *key;
    int value;
    Node *next;
};

struct Chain
{
    Node *head;
    int length;
};

struct HashMap
{
    Chain *chains;
    int chainsNumber;
    int filledChains;
    int nodes;

    Chain *newChains;
    int newChainsNumber;
    int newFilledChains;
    int newNodes;

    float expansionFactor;

    HashFunction *hash;

    bool somethingUsefulIsDoing;
    int currentChain;
};

Node *createNode(String *key, int value)
{
    Node *node = new Node;
    node->key = clone(key);
    node->value = value;
    node->next = nullptr;
    return node;
}

void deleteNode(Node *&node)
{
    deleteString(node->key);
    delete node;
    node = nullptr;
}

void clearChain(Chain &chain)
{
    while (chain.head != nullptr)
    {
        Node *toDelete = chain.head;
        chain.head = chain.head->next;
        deleteNode(toDelete);
    }

    chain.length = 0;
}

HashMap *createHashMap(HashFunction *hash)
{
    const float expansionFactor = 1.618f;
    const int initialChains = 29;

    HashMap *map = new HashMap;

    map->hash = hash;
    map->expansionFactor = expansionFactor;

    map->filledChains = 0;
    map->chainsNumber = initialChains;
    map->chains = new Chain[map->chainsNumber];
    map->nodes = 0;
    for (int i = 0; i < map->chainsNumber; i++)
    {
        map->chains[i].head = nullptr;
        map->chains[i].length = 0;
    }

    map->newFilledChains = 0;
    map->newChainsNumber = 0;
    map->newChains = nullptr;
    map->newNodes = 0;

    map->somethingUsefulIsDoing = false;
    map->currentChain = 0;

    return map;
}

void deleteChainMap(Chain *&chains, int chainsNumber)
{
    for (int i = 0; i < chainsNumber; i++)
        clearChain(chains[i]);

    delete[] chains;
    chains = nullptr;
}

void deleteHashMap(HashMap *&map)
{
    deleteChainMap(map->chains, map->chainsNumber);

    if (map->newChains != nullptr)
        deleteChainMap(map->newChains, map->newChainsNumber);

    delete map;
    map = nullptr;
}

void switchChains(HashMap *map)
{
    deleteChainMap(map->chains, map->chainsNumber);

    map->chains = map->newChains;
    map->chainsNumber = map->newChainsNumber;
    map->filledChains = map->newFilledChains;
    map->nodes = map->newNodes;

    map->newChains = nullptr;
    map->newChainsNumber = 0;
    map->newFilledChains = 0;
    map->newNodes = 0;

    map->currentChain = 0;
}

void transferChain(HashMap *map, Chain &chain)
{
    Node *cursor = chain.head;
    while (cursor != nullptr)
    {
        add(map, cursor->key, cursor->value);

        Node *toDelete = cursor;
        cursor = cursor->next;
        deleteNode(toDelete);
    }

    chain.head = nullptr;
    chain.length = 0;
}

void fullTransfer(HashMap *map)
{
    for (int i = map->currentChain; i < map->chainsNumber; i++)
        transferChain(map, map->chains[i]);

    switchChains(map);
}

void doSomethingWithNewChains(HashMap *map)
{
    const float dangerousNewBufferFactor = 0.4f;

    float loadFactor = ((float) map->newNodes) / map->newChainsNumber;

    if (loadFactor > dangerousNewBufferFactor)
    {
        fullTransfer(map);
    }
    else
    {
        while (map->currentChain < map->chainsNumber && map->chains[map->currentChain].length == 0)
            map->currentChain++;

        if (map->currentChain == map->chainsNumber)
            switchChains(map);
        else
        {
            transferChain(map, map->chains[map->currentChain]);
            map->filledChains--;
        }
    }
}

void doSomethingWithOldChains(HashMap *map)
{
    const float transferThreshold = 0.6f;

    float loadFactor = ((float) map->nodes) / map->chainsNumber;

    if (loadFactor > transferThreshold)
    {
        int newChainsNumber = map->chainsNumber * map->expansionFactor;

        map->newChains = new Chain[newChainsNumber];
        map->newChainsNumber = newChainsNumber;
        map->newFilledChains = 0;
        for (int i = 0; i < map->newChainsNumber; i++)
        {
            map->newChains[i].head = nullptr;
            map->newChains[i].length = 0;
        }
    }
}

void doSomethingUseful(HashMap *map)
{
    map->somethingUsefulIsDoing = true;

    if (map->newChains != nullptr)
    {
        doSomethingWithNewChains(map);
    }
    else
    {
        doSomethingWithOldChains(map);
    }

    map->somethingUsefulIsDoing = false;
}

bool add(Chain &chain, String *key, int value)
{
    if (chain.head == nullptr)
    {
        chain.head = createNode(key, value);
        chain.length++;
        return true;
    }

    Node *cursor = chain.head;
    do
    {
        if (equals(key, cursor->key))
            return false;

        if (cursor->next != nullptr)
            cursor = cursor->next;
    } while (cursor->next != nullptr);

    cursor->next = createNode(key, value);
    chain.length++;

    return true;
}

bool add(Chain *chains, int chainsNumber, int &filledChains, String *key, int value, HashFunction *hash)
{
    int position = hash(castToChars(key)) % chainsNumber;

    if (chains[position].length == 0)
        filledChains++;

    return add(chains[position], key, value);
}

bool add(HashMap *map, String *key, int value)
{
    bool status = false;
    if (map->newChains == nullptr)
    {
        status = add(map->chains, map->chainsNumber, map->filledChains, key, value, map->hash);
        if (status)
            map->nodes++;
    }
    else
    {
        status = add(map->newChains, map->newChainsNumber, map->newFilledChains, key, value, map->hash);
        if (status)
            map->newNodes++;
    }

    if (!map->somethingUsefulIsDoing)
        doSomethingUseful(map);

    return status;
}

bool add(HashMap *map, const char *key, int value)
{
    String *string = createString(key);
    bool status = add(map, string, value);
    deleteString(string);
    return status;
}

void remove(Node *&node)
{
    Node *toDelete = node;
    node = node->next;
    deleteNode(toDelete);
}

bool remove(Chain &chain, String *key)
{
    if (chain.head == nullptr)
        return false;

    if (equals(chain.head->key, key))
    {
        remove(chain.head);
        chain.length--;
        return true;
    }

    Node *cursor = chain.head;
    while (cursor->next != nullptr)
    {
        if (equals(cursor->next->key, key))
        {
            remove(cursor->next);
            chain.length--;
            return true;
        }

        cursor = cursor->next;
    }

    return false;
}

bool remove(Chain *chains, int chainsNumber, int &filledChains, String *key, HashFunction *hash)
{
    int position = hash(castToChars(key)) % chainsNumber;

    bool status = remove(chains[position], key);

    if (status && chains[position].length == 0)
        filledChains--;

    return status;
}

bool remove(HashMap *map, String *key)
{
    bool status = false;

    status = remove(map->chains, map->chainsNumber, map->filledChains, key, map->hash);

    if (status)
        map->nodes--;
    else
    {
        status = remove(map->newChains, map->newChainsNumber, map->newFilledChains, key, map->hash);
        if (status)
            map->newNodes--;
    }

    if (!map->somethingUsefulIsDoing)
        doSomethingUseful(map);

    return status;
}

bool remove(HashMap *map, const char *key)
{
    String *string = createString(key);
    bool status = remove(map, string);
    deleteString(string);
    return status;
}

int getValue(Chain &chain, String *key)
{
    Node *cursor = chain.head;

    while (cursor != nullptr)
    {
        if (equals(cursor->key, key))
            return cursor->value;

        cursor = cursor->next;
    }

    return INT_MIN;
}

int getValue(Chain *chains, int chainsNumber, String *key, HashFunction *hash)
{
    int position = hash(castToChars(key)) % chainsNumber;

    return getValue(chains[position], key);
}

int getValue(HashMap *map, String *key)
{
    int value = getValue(map->chains, map->chainsNumber, key, map->hash);

    if (value == INT_MIN && map->newChains != nullptr)
        value = getValue(map->newChains, map->newChainsNumber, key, map->hash);

    if (!map->somethingUsefulIsDoing)
        doSomethingUseful(map);

    return value;
}

int getValue(HashMap *map, const char *key)
{
    String *string = createString(key);
    int value = getValue(map, string);
    deleteString(string);
    return value;
}

bool setValue(Chain &chain, String *key, int value)
{
    Node *cursor = chain.head;

    while (cursor != nullptr)
    {
        if (equals(cursor->key, key))
        {
            cursor->value = value;
            return true;
        }

        cursor = cursor->next;
    }

    return false;
}

bool setValue(Chain *chains, int chainsNumber, String *key, int value, HashFunction *hash)
{
    int position = hash(castToChars(key)) % chainsNumber;

    return setValue(chains[position], key, value);
}

bool setValue(HashMap *map, String *key, int value)
{
    bool status = setValue(map->chains, map->chainsNumber, key, value, map->hash);

    if (!status && map->newChains != nullptr)
        status = setValue(map->newChains, map->newChainsNumber, key, value, map->hash);

    if (!map->somethingUsefulIsDoing)
        doSomethingUseful(map);

    return status;
}

bool setValue(HashMap *map, const char *key, int value)
{
    String *string = createString(key);
    bool status = setValue(map, string, value);
    deleteString(string);
    return status;
}

int size(HashMap *map)
{
    return map->nodes + map->newNodes;
}

float loadFactor(HashMap *map)
{
    return ((float) (map->nodes + map->newNodes)) / (map->chainsNumber + map->newChainsNumber);
}

float averageChainLength(HashMap *map)
{
    uint64_t sum = 0;

    for (int i = 0; i < map->chainsNumber; i++)
        sum += map->chains[i].length;

    if (map->newChains != nullptr)
    {
        for (int i = 0; i < map->newChainsNumber; i++)
            sum += map->newChains[i].length;
    }

    return ((float) sum) / (map->filledChains + map->newFilledChains);
}

int maximumChainLength(HashMap *map)
{
    int max = 0;

    for (int i = 0; i < map->chainsNumber; i++)
        if (map->chains[i].length > max)
            max = map->chains[i].length;

    if (map->newChains != nullptr)
    {
        for (int i = 0; i < map->newChainsNumber; i++)
            if (map->newChains[i].length > max)
                max = map->newChains[i].length;
    }

    return max;
}

int emptyChainsNumber(HashMap *map)
{
    return map->chainsNumber + map->newChainsNumber - map->filledChains - map->newFilledChains;
}

void getLongestChain(HashMap *map, String **keys, int *values)
{
    int max = -1;
    Chain *chain = nullptr;

    for (int i = 0; i < map->chainsNumber; i++)
        if (map->chains[i].length > max)
        {
            chain = &map->chains[i];
            max = map->chains[i].length;
        }

    if (map->newChains != nullptr)
    {
        for (int i = 0; i < map->newChainsNumber; i++)
            if (map->newChains[i].length > max)
            {
                chain = &map->newChains[i];
                max = map->newChains[i].length;
            }
    }

    Node *cursor = chain->head;
    for (int i = 0; i < max; i++)
    {
        keys[i] = clone(cursor->key);
        values[i] = cursor->value;
        cursor = cursor->next;
    }
}
