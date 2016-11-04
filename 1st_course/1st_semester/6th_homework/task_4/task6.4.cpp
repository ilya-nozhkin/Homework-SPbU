#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

int countNotEmptyLines(fstream &file)
{
    int count = 0;
    while (!file.eof())
    {
        const int bufferSize = 1024;
        char buffer[bufferSize] = {0};
        file.getline(buffer, bufferSize);
        
        bool empty = true;
        int cursor = 0;
        int length = strlen(buffer);
        
        bool process = true;
        while (process)
        {
            if (cursor >= length)
                process = false;
            else
            {
                if (buffer[cursor] != '\t' && buffer[cursor] != ' ' && 
                    buffer[cursor] != '\r' && buffer[cursor] != '\n')
                {
                    empty = false;
                    process = false;
                }
            }
            cursor++;
        }
        
        if (!empty)
            count++;
    }
    
    return count;
}

int main()
{
    const int fileNameSize = 256;
    char fileName[fileNameSize] = {0};
    inputString(fileName, "the file name");
    
    fstream file(fileName);
    
    if (!file.is_open())
    {
        cout << "Couldn't open the file" << endl;
        return 1;
    }
        
    int lines = countNotEmptyLines(file);
    cout << "The number of not empty lines in the file: " << lines << endl;
        
    file.close();
    
    return 0;
}
