#include "StateMachine.h"

#include <cstring>

struct TransitionRule
{
    static const int maxInputChars = 64;
    char input[maxInputChars];
    int *transitions;
};

struct StateMachine
{
    int state;
    TransitionRule *rules;
    int rulesNumber;
    int stateNumber;
};

StateMachine *createStateMachine()
{
    StateMachine *machine = new StateMachine;
    machine->state = 0;
    machine->rules = nullptr;
    machine->rulesNumber = 0;
    return machine;
}

void deleteStateMachine(StateMachine *&machine)
{
    if (machine->rules != nullptr)
    {
        for (int i = 0; i < machine->rulesNumber; i++)
            delete[] machine->rules[i].transitions;
        delete[] machine->rules;
    }
    delete machine;
    machine = nullptr;
}

void deserialize(StateMachine *machine, std::istream &stream)
{
    stream >> machine->stateNumber;
    stream >> machine->rulesNumber;

    machine->rules = new TransitionRule[machine->rulesNumber];

    for (int i = 0; i < machine->rulesNumber; i++)
    {
        machine->rules[i].transitions = new int[machine->stateNumber];

        stream >> machine->rules[i].input;

        for (int j = 0; j < machine->stateNumber; j++)
            stream >> machine->rules[i].transitions[j];
    }
}

void setState(StateMachine *machine, int state)
{
    machine->state = state;
}

int getState(StateMachine *machine)
{
    return machine->state;
}

bool ruleMatch(TransitionRule &rule, char input)
{
    for (unsigned int i = 0; i < strlen(rule.input); i++)
        if (rule.input[i] == input)
            return true;
    return false;
}

void step(StateMachine *machine, char input)
{
    int rule = 0;
    bool process = true;
    while (process && rule < machine->rulesNumber - 1)
    {
        bool match = false;
        if (rule == machine->rulesNumber - 1)
            match = true;
        else
            match = ruleMatch(machine->rules[rule], input);

        if (match)
        {
            machine->state = machine->rules[rule].transitions[machine->state];
            process = false;
        }

        rule++;
    }
}
