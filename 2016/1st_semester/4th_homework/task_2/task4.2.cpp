#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

void inputString(char *buffer, const char *what)
{
	cout << "Enter " << what << ": ";
	cin >> buffer;
}

void removeUnnecessaryLetters(char *word)
{
	const int byteValues = 256;
	bool found[byteValues] = {false};
	char queue[byteValues] = {'\0'};
	int cursor = 0;
	
	int length = strlen(word);
	for (int i = 0; i < length; i++)
	{
		char current = word[i];
		if (!found[(int) current])
		{
			found[(int) current] = true;
			queue[cursor] = current;
			cursor++;
		}
	}
	
	strcpy(word, queue);
}

int main()
{
	const int fileNameSize = 256;
	char fileName[fileNameSize] = "";
	inputString(fileName, "the file name (it should not contain spaces)");
	
	ifstream file(fileName);
	
	if (!file.is_open())
	{
		cout << "couldn't open the file \"" << fileName << "\"" << endl;
		return 1;
	}
	
	const int wordSize = 512;
	char word[wordSize] = {'\0'};
	while (!file.eof())
	{
		file >> word;
		removeUnnecessaryLetters(word);
		cout << word << ' ';
	}
	cout << endl;
	
	file.close();
	
	return 0;
}
