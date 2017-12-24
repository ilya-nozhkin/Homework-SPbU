#pragma once

#include <cppunit/TestCase.h>

class SpontaneousVirusGenerationTest : public CppUnit::TestCase
{
public:
    void runTest();
};

class BackpropagationTest : public CppUnit::TestCase
{
public:
    void runTest();
};

class InfectionOrderTest : public CppUnit::TestCase
{
public:
    void runTest();
};

class VulnerabilitiesTest : public CppUnit::TestCase
{
public:
    void runTest();
};

bool runTests();
