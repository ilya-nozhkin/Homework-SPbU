#pragma once

#include <istream>

struct StateMachine;

StateMachine *createStateMachine();
void deleteStateMachine(StateMachine *&machine);

void deserialize(StateMachine *machine, std::istream &stream);

void setState(StateMachine *machine, int state);
int  getState(StateMachine *machine);
void step(StateMachine *machine, char input);
