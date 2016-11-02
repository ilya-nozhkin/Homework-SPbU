#include <iostream>
#include <cstdlib>

using namespace std;

bool contains(int number, int size, int digit)
{
	for (int i = 0; i < size; i++)
	{
		if (digit == number % 10)
			return true;
		number /= 10;
	}
	return false;
}

bool areAllDigitsUnique(int number, int digits)
{
	bool flags[10] = {false};
	for (int i = 0; i < digits; i++)
	{
		int digit = number % 10;
		if (flags[digit])
			return false;
		flags[digit] = true;
		number /= 10;
	}
	return true;
}

int inputUniqueDigits(const char *what, int digits)
{
	int number = 0;
	bool process = true;
	while (process)
	{
		cout << "Enter " << what << " (" << digits << " digits): ";
		cin >> number;
		
		if (areAllDigitsUnique(number, digits))
		{
			process = false;
		}
		else
		{
			cout << "digits shouldn't be repeated" << endl;
		}
	}
	return number;
}

bool turn(int secret, int digits)
{
	int answer = inputUniqueDigits("your number", digits);
	
	if (secret == answer)
	{
		cout << "You have guessed!!!" << endl;
		return true;
	}
	else
	{
		int bulls = 0;
		int cows = 0;
		
		int storedAnswer = answer;
		for (int i = 0; i < digits; i++)
		{
			if (contains(secret, digits, storedAnswer % 10))
				cows++;
			storedAnswer /= 10;
		}
		
		storedAnswer = answer;
		int storedSecret = secret;
		for (int i = 0; i < digits; i++)
		{
			if (storedSecret % 10 == storedAnswer % 10)
				bulls++;
				
			storedAnswer /= 10;
			storedSecret /= 10;
		}
		
		cows -= bulls;
		
		cout << cows << " cows and " << bulls << " bulls" << endl;
		return false;
	}
}

int generateSecret(int digits)
{
	int secret = 0;
	bool flags[10] = {false};
	for (int i = 0; i < digits; i++)
	{
		bool process = true;
		while (process)
		{
			int digit = rand() % 10;
			if (!flags[digit])
			{
				secret = secret * 10 + digit;
				flags[digit] = true;
				process = false;
			}
		}
	}
	return secret;
}

void singlePlayer(int digits)
{
	int secret = generateSecret(digits);
	
	cout << "Program has generated some number, try to guess it" << endl;
	
	bool process = true;
	while (process)
	{
		if (turn(secret, digits))
			process = false;
	}
}

void clearWindow()
{
	for (int i = 0; i < 1000; i++)
		cout << endl;
}

const char *getPlayersString(int id)
{
	return (id == 0 ? "1'st" : "2'nd");
}

void multiPlayer(int digits)
{
	int secrets[2] = {0};
	
	for (int i = 0; i < 2; i++)
	{
		clearWindow();
		cout << getPlayersString(i) << " player's turn: ";
		secrets[i] = inputUniqueDigits("your secret number", digits);
	}
	
	clearWindow();
	
	int order = 0;
	bool process = true;
	while (process)
	{
		int next = (order + 1) % 2;
		
		cout << getPlayersString(order);
		cout << " player's turn" << endl;
		
		if (turn(secrets[next], digits))
		{
			cout << getPlayersString(order);
			cout << " player has won!!!";
			process = false;
		}
		
		cout << endl;
		
		order = next;
	}
}

int main()
{
	srand(time(nullptr));
	
	const int digits = 4;
	
	int mode = 0;
	cout << "Choose the game mode (1 - single player, 2 - multi player): ";
	cin >> mode;
	
	if (mode == 1)
		singlePlayer(digits);
	else
		multiPlayer(digits);
	

	return 0;
}
