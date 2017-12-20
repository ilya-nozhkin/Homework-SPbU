#include "tests.h"
#include "network.h"

#include <cppunit/ui/text/TextTestRunner.h>

void SpontaneousVirusGenerationTest::runTest()
{
    FileComputerFactory factory("healthyNetwork.txt");
    Router router(factory);

    const int ticks = 500;
    for (int i = 0; i < ticks; i++)
    {
        router.tick();
    }

    auto snapshot = router.scan();
    for (auto pair : snapshot)
    {
        CPPUNIT_ASSERT_MESSAGE(
                    "Spontaneous virus generation test failed: viruses are found in originally healthy network",
                    !pair.second);
    }
}

void InfectionOrderTest::runTest()
{
    FileComputerFactory factory("infectedNetwork.txt");
    Router router(factory);

    const int chain[] = {1, 4, 6, 9};
    const int chainLength = 4;

    CPPUNIT_ASSERT_MESSAGE(
                "Infection order test failed: first computer in a chain is not infected",
                router.scan()[chain[0]]);

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

            CPPUNIT_ASSERT_MESSAGE(
                        "Infection order test failed: viruses penetrated too far",
                        infected || !snapshot[chain[i]]);
        }
    }
}

void BackpropagationTest::runTest()
{
    FileComputerFactory factory("anotherInfectedNetwork.txt");
    Router router(factory);

    const int chain[] = {1, 4, 6, 9, 10};
    const int chainLength = 5;

    CPPUNIT_ASSERT_MESSAGE(
                "Backpropagation test failed: last computer in a chain is not infected",
                 router.scan()[chain[chainLength - 1]]);

    const int ticks = 100;
    for (int i = 0; i < ticks; i++)
    {
        router.tick();
        auto snapshot = router.scan();
        for (int j = 0; j < chainLength - 1; j++)
        {
            CPPUNIT_ASSERT_MESSAGE("Backpropagation test failed: viruses penetrated into unconnected computer",
                                   !snapshot[chain[j]]);
        }
    }
}

void VulnerabilitiesTest::runTest()
{
    const uint64_t windowsAddress = 4;
    const uint64_t linuxAddress = 2;
    const uint64_t macosAddress = 3;
    const int attempts = 4000;

    int windowsInfectionCounter = 0;
    int linuxInfectionCounter = 0;
    int macosInfectionCounter = 0;

    for (int i = 0; i < attempts; i++)
    {
        FileComputerFactory factory("infectedNetwork.txt");
        Router router(factory);

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

    CPPUNIT_ASSERT_DOUBLES_EQUAL_MESSAGE(
                "Vulnerabilities test failed: chance of infection on Windows is wrong",
                expectedWindowsInfectionChance, windowsInfectionChance, epsilon);

    CPPUNIT_ASSERT_DOUBLES_EQUAL_MESSAGE(
                "Vulnerabilities test failed: chance of infection on Linux is wrong",
                expectedLinuxInfectionChance, linuxInfectionChance, epsilon);

    CPPUNIT_ASSERT_DOUBLES_EQUAL_MESSAGE(
                "Vulnerabilities test failed: chance of infection on MacOS is wrong",
                expectedMacosInfectionChance, macosInfectionChance, epsilon);
}

bool runTests()
{
    CppUnit::TextTestRunner runner;

    runner.addTest(new SpontaneousVirusGenerationTest());
    runner.addTest(new InfectionOrderTest());
    runner.addTest(new BackpropagationTest());
    runner.addTest(new VulnerabilitiesTest());

    return runner.run();
}
