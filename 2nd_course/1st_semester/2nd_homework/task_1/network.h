#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>
#include <fstream>
#include <functional>

#include <inttypes.h>

class Computer
{
public:
    Computer(uint64_t address, std::vector<uint64_t> neighbours, bool infected);

    void handshake(std::function<void(uint64_t, std::string)> sendMessage);

    void receiveMessage(const std::string &message);
    void tick();

    bool scan();
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

class FileComputerFactory : public ComputerFactory
{
public:
    FileComputerFactory(std::string filename);

    std::unique_ptr<Computer> produce();
    bool hasNext();
private:
    std::ifstream stream;
    int number;
    int counter;
};

class Router
{
public:
    Router(ComputerFactory &factory);

    void tick();

    std::map<uint64_t, bool> scan();
private:
    std::map<uint64_t, std::unique_ptr<Computer>> network;
    std::unique_ptr<Computer> connectingComputer;

    void finishConnecting(std::string handshakeMessage);
    void sendMessage(uint64_t address, const std::string &message);
};


class WindowsComputer : public Computer {
public:
    WindowsComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected);
protected:
    void backdoor(const std::string &message);
};

class LinuxComputer : public Computer {
public:
    LinuxComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected);
private:
    void backdoor(const std::string &message);
};

class MacOSComputer : public Computer {
public:
    MacOSComputer(uint64_t address, std::vector<uint64_t> neighbours, bool infected);
private:
    void backdoor(const std::string &message);
};
