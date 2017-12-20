#include "network.h"

#include <sstream>

#include <stdlib.h>

using namespace std;

FileComputerFactory::FileComputerFactory(std::string filename) : ComputerFactory(), stream(filename)
{
    stream >> number;
    counter = 0;
}

bool FileComputerFactory::hasNext()
{
    return counter < number;
}

std::unique_ptr<Computer> FileComputerFactory::produce()
{
    std::string info;
    std::string system;
    std::string infection;
    uint64_t address = 0;

    int numNeighbours = 0;
    std::vector<uint64_t> neighbours;

    stream >> address;
    stream >> system;
    stream >> infection;
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

    bool infected = infection.compare("infected") == 0;

    if (system == "windows")
        return unique_ptr<Computer>(new WindowsComputer(address, neighbours, infected));
    if (system == "linux")
        return unique_ptr<Computer>(new LinuxComputer(address, neighbours, infected));
    if (system == "macos")
        return unique_ptr<Computer>(new MacOSComputer(address, neighbours, infected));

    return nullptr;
}

Router::Router(ComputerFactory &factory) : network()
{
    std::function<void(uint64_t, const std::string&)> sendMessageCallback =
            [&](uint64_t address, std::string message)
            {
                sendMessage(address, message);
            };

    while (factory.hasNext()) {
        connectingComputer = std::move(factory.produce());
        connectingComputer->handshake(sendMessageCallback);
    }
}

void Router::tick()
{
    for (auto iter = network.begin(); iter != network.end(); iter++)
    {
        iter->second->tick();
    }
}


void Router::sendMessage(uint64_t address, const std::string &message)
{
    if (address == 0)
    {
        finishConnecting(message);
    }
    else
    {
        network[address]->receiveMessage(message);
    }
}

void Router::finishConnecting(std::string handshakeMessage) {
    std::istringstream stream(handshakeMessage);
    uint64_t computerAddress = 0;
    stream >> computerAddress;

    network[computerAddress] = std::move(connectingComputer);
}

std::map<uint64_t, bool> Router::scan()
{
    std::map<uint64_t, bool> results;
    for (auto iter = network.begin(); iter != network.end(); iter++)
    {
        results[iter->first] = iter->second->scan();
    }
    return results;
}

Computer::Computer(uint64_t address, std::vector<uint64_t> neighbours, bool infected)
{
    this->address = address;
    this->neighbours = neighbours;
    this->infected = infected;
}

void Computer::handshake(std::function<void(uint64_t, std::string)> sendMessage)
{
    this->sendMessage = sendMessage;
    this->sendMessage(0, std::to_string(address));
}

void Computer::tick()
{
    static const std::string nonInfectedMessage = "hello";
    static const std::string infectedMessage = "virus";
    const std::string &message = infected ? infectedMessage : nonInfectedMessage;

    for (auto neighbour : neighbours)
    {
        sendMessage(neighbour, message);
    }
}

void Computer::receiveMessage(const std::string &message)
{
    if (!infected)
    {
        backdoor(message);
    }
}

bool Computer::scan()
{
    return infected;
}

WindowsComputer::WindowsComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected) :
    Computer(address, neighbours, infected) {}

LinuxComputer::LinuxComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected) :
    Computer(address, neighbours, infected) {}

MacOSComputer::MacOSComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected) :
    Computer(address, neighbours, infected) {}

// These methods should be very dirty hacks, that's why they can contain code which can be as bad as possible

void WindowsComputer::backdoor(const std::string &message) {
    if (message == "virus")
    {
        if (rand() % 100 < 20)
        {
            infected = true;
        }
    }
}

void LinuxComputer::backdoor(const std::string &message) {
    if (message == "virus")
    {
        if (rand() % 100 < 10)
        {
            infected = true;
        }
    }
}

void MacOSComputer::backdoor(const std::string &message) {
    if (message == "virus")
    {
        if (rand() % 100 < 15)
        {
            infected = true;
        }
    }
}
