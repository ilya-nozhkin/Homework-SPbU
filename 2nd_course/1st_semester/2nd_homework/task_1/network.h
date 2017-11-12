#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>
#include <functional>

#include <inttypes.h>

static const std::string secretVirusCode = "infect_immediately";

class Computer
{
public:
    Computer(uint64_t address, std::vector<uint64_t> neighbours);

    void handshake(std::function<void(uint64_t, std::string)> sendMessage);

    void receiveMessage(const std::string &message);
    void tick();

    bool virusScan();
private:
    std::function<void(uint64_t, std::string)> sendMessage;
    std::vector<uint64_t> neighbours;
    uint64_t address;
protected:
    bool infected;
    virtual void backdoor(const std::string &message) = 0;
};

class ComputerFactory
{
public:
    virtual std::unique_ptr<Computer> produce() = 0;
    virtual bool hasNext() = 0;
};

class Router
{
public:
    Router(ComputerFactory &factory);

    void sendMessage(uint64_t address, const std::string &message);
    void tick();

    std::map<uint64_t, bool> virusScan();
private:
    std::map<uint64_t, std::unique_ptr<Computer>> network;
    std::unique_ptr<Computer> connectingComputer;

    void finishConnecting(std::string handshakeMessage);
};


class WindowsComputer : public Computer {
public:
    WindowsComputer(uint64_t address, std::vector<uint64_t> neighbours);
protected:
    void backdoor(const std::string &message);
};

class LinuxComputer : public Computer {
public:
    LinuxComputer(uint64_t address, std::vector<uint64_t> neighbours);
private:
    void backdoor(const std::string &message);
};

class MacOSComputer : public Computer {
public:
    MacOSComputer(uint64_t address, std::vector<uint64_t> neighbours);
private:
    void backdoor(const std::string &message);
};
