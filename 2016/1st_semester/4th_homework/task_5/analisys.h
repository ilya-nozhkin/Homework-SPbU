#pragma once

bool isItNumber(const char *expression, int position);
bool isItOperator(const char *expression, int position);
int getOperatorPriority(char character);
