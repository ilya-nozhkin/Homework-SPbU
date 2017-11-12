#include <iostream>
#include <fstream>
#include <chrono>

#include <stdlib.h>

#include "network.h"

using namespace std;

class FileComputerFactory : public ComputerFactory
{
public:
    FileComputerFactory(std::string filename) : ComputerFactory(), stream(filename)
    {
        stream >> number;
        counter = 0;
    }

    bool hasNext()
    {
        return counter < number;
    }

    std::unique_ptr<Computer> produce()
    {
        std::string info;
        std::string system;
        uint64_t address = 0;

        int numNeighbours = 0;
        std::vector<uint64_t> neighbours;

        stream >> address;
        stream >> system;
        stream >> info;
        stream >> numNeighbours;
        stream >> info;

        for (int i = 0; i < numNeighbours; i++)
        {
            uint64_t neighbour = 0;
            stream >> neighbour;
            neighbours.push_back(neighbour);
        }

        counter++;

        if (system == "windows")
            return unique_ptr<Computer>(new WindowsComputer(address, neighbours));
        if (system == "linux")
            return unique_ptr<Computer>(new LinuxComputer(address, neighbours));
        if (system == "macos")
            return unique_ptr<Computer>(new MacOSComputer(address, neighbours));

        return nullptr;
    }
private:
    ifstream stream;
    int number;
    int counter;
};

void printSnapshot(Router &router)
{
    auto snapshot = router.virusScan();
    for (auto pair : snapshot)
    {
        std::cout << pair.first << " - " << (pair.second ? "infected" : "not infected") << std::endl;
    }
}

bool checkIfAllInfected(Router &router)
{
    auto snapshot = router.virusScan();
    for (auto pair : snapshot)
    {
        if (!pair.second)
        {
            return false;
        }
    }

    return true;
}

bool testThatThereIsNoSpontaneousVirusGeneration()
{
    FileComputerFactory factory("test.txt");
    Router router(factory);

    for (int i = 0; i < 500; i++)
    {
        router.tick();
    }

    auto snapshot = router.virusScan();
    for (auto pair : snapshot)
    {
        if (pair.second)
        {
            return false;
        }
    }

    return true;
}

bool testInfectionOrder()
{
    FileComputerFactory factory("test.txt");
    Router router(factory);

    const int chain[] = {1, 4, 6, 9};
    const int chainLength = 4;

    router.sendMessage(chain[0], secretVirusCode);
    if (!router.virusScan()[chain[0]])
    {
        return false;
    }

    for (int i = 0; i < 100; i++)
    {
        router.tick();
        auto snapshot = router.virusScan();

        bool infected = snapshot[chain[0]];
        for (int i = 1; i < chainLength; i++)
        {
            if (!snapshot[chain[i]])
            {
                infected = false;
            }

            if (!infected && snapshot[chain[i]])
            {
                return false;
            }
        }
    }

    return true;
}

bool testThatThereIsNoBackpropagation()
{
    FileComputerFactory factory("test.txt");
    Router router(factory);

    const int chain[] = {1, 4, 6, 9, 10};
    const int chainLength = 5;

    router.sendMessage(chain[chainLength - 1], secretVirusCode);
    if (!router.virusScan()[chain[chainLength - 1]])
    {
        return false;
    }

    for (int i = 0; i < 100; i++)
    {
        router.tick();
        auto snapshot = router.virusScan();
        for (int j = 0; j < chainLength - 1; j++)
        {
            if (snapshot[chain[j]])
            {
                return false;
            }
        }
    }

    return true;
}

int main()
{
    srand(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));

    if (!testInfectionOrder())
    {
        std::cout << "Infection order test failed" << std::endl;
    }

    if (!testThatThereIsNoSpontaneousVirusGeneration())
    {
        std::cout << "Spontaneous virus generation test failed" << std::endl;
    }

    if (!testThatThereIsNoBackpropagation())
    {
        std::cout << "Backpropagation test failed" << std::endl;
    }

    FileComputerFactory factory("example.txt");
    Router router(factory);

    router.sendMessage(1, secretVirusCode);

    bool process = true;
    int step = 0;
    while (process)
    {
        router.tick();

        std::cout << "step: " << step << std::endl;
        printSnapshot(router);

        process = !checkIfAllInfected(router);

        step++;
    }

    return 0;
}

