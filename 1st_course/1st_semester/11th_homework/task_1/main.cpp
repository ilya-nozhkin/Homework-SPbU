#include "StateMachine.h"

#include <fstream>
#include <iostream>
#include <cstring>

using namespace std;

StateMachine *createRealChecker()
{
    StateMachine *machine = createStateMachine();

    const char *realFSM = "realFSM.txt";
    fstream file(realFSM);
    if (!file.is_open())
    {
        cout << "couldn't open state machine's configuration file '" << realFSM << "'" << endl;
        return nullptr;
    }

    deserialize(machine, file);

    file.close();

    return machine;
}

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

bool checkReal(StateMachine *machine, char *number)
{
    const int initialState = 0;
    const int finalStateNumber = 3;
    const int finalStates[finalStateNumber] = {1, 3, 6};

    setState(machine, initialState);
    for (unsigned int i = 0; i < strlen(number); i++)
        step(machine, number[i]);

    for (int i = 0; i < finalStateNumber; i++)
        if (getState(machine) == finalStates[i])
            return true;

    return false;
}

int main()
{
    StateMachine *realChecker = createRealChecker();

    if (realChecker == nullptr)
        return 1;

    const int bufferSize = 256;
    char buffer[bufferSize] = {'\0'};
    inputString(buffer, "real number");

    bool status = checkReal(realChecker, buffer);
    if (status)
        cout << "It is a real number" << endl;
    else
        cout << "It is not a real number" << endl;

    deleteStateMachine(realChecker);

    return 0;
}
