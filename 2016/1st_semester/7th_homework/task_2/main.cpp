#include <iostream>
#include <fstream>

#include "CalculatingTree.h"

using namespace std;

void inputString(char *buffer, const char *what)
{
    cout << "Enter " << what << ": ";
    cin >> buffer;
}

int main()
{
    const int fileNameLength = 256;
    char fileName[fileNameLength];
    inputString(fileName, "the name of file");

    ifstream file(fileName);

    if (file.is_open())
    {
        CalculatingTree *tree = createCalculatingTree();

        parse(tree, file);

        printInfixForm(tree, cout);
        cout << " = " << calculate(tree) << endl;

        deleteTree(tree);

        file.close();
    }
    else
    {
        cout << "couldn't open the file" << endl;
    }

    return 0;
}
