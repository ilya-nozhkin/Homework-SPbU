#include <iostream>
#include <fstream>
#include <chrono>

#include <stdlib.h>
#include <math.h>

#include "network.h"
#include "tests.h"

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

int main()
{
    srand(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));

    if (runTests())
    {
        FileComputerFactory factory("infectedNetwork.txt");
        Router router(factory);

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

