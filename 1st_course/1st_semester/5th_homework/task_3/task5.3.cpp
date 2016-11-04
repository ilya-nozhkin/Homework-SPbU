#include <iostream>
#include <fstream>

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ":" << endl;
    cin >> buffer;
}

void printLineFromFile(ifstream &file)
{
    const int bufferSize = 512;
    char buffer[bufferSize] = {'\0'};

    file.getline(buffer, bufferSize);
    cout << buffer << endl;
}

void processFile(ifstream &file)
{
    bool comment = false;
    bool quotes = false;
    while (!file.eof())
    {
        char current = 0;
        file >> current;

        if (!quotes && !comment)
        {
            if (current == '"')
                quotes = true;
            else if (current == '/')
            {
                if (!file.eof())
                {
                    char next = 0;
                    file >> next;

                    if (next == '*')
                        comment = true;
                    else if (next == '/')
                    {
                        cout << "//";
                        printLineFromFile(file);
                    }
                }
            }
        }
        else
        {
            if (current == '"' && quotes)
                quotes = false;
            else if (current == '*' && comment)
            {
                if (!file.eof())
                {
                    char next = 0;
                    file >> next;

                    if (next == '/')
                        comment = false;
                }
            }
        }
    }
}

int main()
{
    const int filenameSize = 256;
    char filename[filenameSize] = {'\0'};
    inputString(filename, "the file name");

    ifstream file(filename);

    if (!file.is_open())
    {
        cout << "couldn't open the file" << endl;
        return 1;
    }

    processFile(file);

    file.close();

    return 0;
}
