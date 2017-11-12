#include "network.h"

#include <sstream>

#include <stdlib.h>

static const std::string secretVirusCode = "infect_immediately";

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

void Router::forceInfect(uint64_t address)
{
    sendMessage(address, secretVirusCode);
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

Computer::Computer(uint64_t address, std::vector<uint64_t> neighbours)
{
    this->address = address;
    this->neighbours = neighbours;
    infected = false;
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

void Computer::backdoor(const std::string &message)
{
    if (message == secretVirusCode)
    {
        infected = true;
    }
}

WindowsComputer::WindowsComputer(uint64_t address, std::vector<uint64_t> neighbours) : Computer(address, neighbours) {}
LinuxComputer::LinuxComputer(uint64_t address, std::vector<uint64_t> neighbours) : Computer(address, neighbours) {}
MacOSComputer::MacOSComputer(uint64_t address, std::vector<uint64_t> neighbours) : Computer(address, neighbours) {}

// These methods should be very dirty hacks, that's why they can contain code which can be as bad as possible

void WindowsComputer::backdoor(const std::string &message) {
    Computer::backdoor(message);
    if (message == "virus")
    {
        if (rand() % 100 < 20)
        {
            infected = true;
        }
    }
}

void LinuxComputer::backdoor(const std::string &message) {
    Computer::backdoor(message);
    if (message == "virus")
    {
        if (rand() % 100 < 10)
        {
            infected = true;
        }
    }
}

void MacOSComputer::backdoor(const std::string &message) {
    Computer::backdoor(message);
    if (message == "virus")
    {
        if (rand() % 100 < 15)
        {
            infected = true;
        }
    }
}
