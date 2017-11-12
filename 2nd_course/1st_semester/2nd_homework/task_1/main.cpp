#include <iostream>
#include <fstream>
#include <chrono>

#include <stdlib.h>
#include <math.h>

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
    auto snapshot = router.scan();
    for (auto pair : snapshot)
    {
        std::cout << pair.first << " - " << (pair.second ? "infected" : "not infected") << std::endl;
    }
}

bool checkIfAllInfected(Router &router)
{
    auto snapshot = router.scan();
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

    const int ticks = 500;
    for (int i = 0; i < ticks; i++)
    {
        router.tick();
    }

    auto snapshot = router.scan();
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

    router.forceInfect(chain[0]);
    if (!router.scan()[chain[0]])
    {
        return false;
    }

    const int ticks = 100;
    for (int i = 0; i < ticks; i++)
    {
        router.tick();
        auto snapshot = router.scan();

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

    router.forceInfect(chain[chainLength - 1]);
    if (!router.scan()[chain[chainLength - 1]])
    {
        return false;
    }

    const int ticks = 100;
    for (int i = 0; i < ticks; i++)
    {
        router.tick();
        auto snapshot = router.scan();
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

bool testForVulnerabilities()
{
    const uint64_t windowsAddress = 4;
    const uint64_t linuxAddress = 2;
    const uint64_t macosAddress = 3;
    const uint64_t zeroPatient = 1;
    const int attempts = 4000;

    int windowsInfectionCounter = 0;
    int linuxInfectionCounter = 0;
    int macosInfectionCounter = 0;

    for (int i = 0; i < attempts; i++)
    {
        FileComputerFactory factory("test.txt");
        Router router(factory);

        router.forceInfect(zeroPatient);
        router.tick();

        auto snapshot = router.scan();

        windowsInfectionCounter += snapshot[windowsAddress] ? 1 : 0;
        linuxInfectionCounter += snapshot[linuxAddress] ? 1 : 0;
        macosInfectionCounter += snapshot[macosAddress] ? 1 : 0;
    }

    double windowsInfectionChance = (double) windowsInfectionCounter / attempts;
    double linuxInfectionChance = (double) linuxInfectionCounter / attempts;
    double macosInfectionChance = (double) macosInfectionCounter / attempts;

    const double expectedWindowsInfectionChance = 0.2;
    const double expectedLinuxInfectionChance = 0.1;
    const double expectedMacosInfectionChance = 0.15;
    const double epsilon = 0.01;

    return fabs(windowsInfectionChance - expectedWindowsInfectionChance) < epsilon &&
           fabs(linuxInfectionChance - expectedLinuxInfectionChance) < epsilon &&
           fabs(macosInfectionChance - expectedMacosInfectionChance) < epsilon;
}

bool testSuite()
{
    if (!testInfectionOrder())
    {
        std::cout << "Infection order test failed" << std::endl;
        return false;
    }

    if (!testThatThereIsNoSpontaneousVirusGeneration())
    {
        std::cout << "Spontaneous virus generation test failed" << std::endl;
        return false;
    }

    if (!testThatThereIsNoBackpropagation())
    {
        std::cout << "Backpropagation test failed" << std::endl;
        return false;
    }

    if (!testForVulnerabilities())
    {
        std::cout << "Vulnerability test failed" << std::endl;
        return false;
    }

    return true;
}

int main()
{
    srand(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));

    if (testSuite())
    {
        FileComputerFactory factory("example.txt");
        Router router(factory);

        const int zeroPatient = 1;
        router.forceInfect(zeroPatient);

        std::cout << "initial state: " << std::endl;
        printSnapshot(router);

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
    }

    return 0;
}

