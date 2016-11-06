#include <iostream>

using namespace std;

int inputNumber(const char *what)
{
    int value = 0;
    cout << "Enter " << what << ": ";
    cin >> value;
    return value;
}

struct Student
{
    int source;
};

void initializeStudents(Student *students, int number)
{
    for (int i = 1; i <= number; i++)
        students[i].source = 0;
}

void inputCopies(Student *students)
{
    int numCopies = inputNumber("the number of students who has copied the task");

    cout << "enter the list of these students (id and source divided by space):" << endl;
    for (int i = 0; i < numCopies; i++)
    {
        int code = 0;
        int source = 0;

        cin >> code >> source;

        students[code].source = source;
    }
}

const char *getNumeral(int number)
{
    if (number == 1)
        return "1-st";
    if (number == 2)
        return "2-nd";
    if (number == 3)
        return "3-rd";
    return "unknown";
}

void printStudents(Student *students, int number)
{
    cout << "students' status:" << endl;
    for (int i = 1; i <= number; i++)
    {
        cout << i << " - ";
        if (students[i].source > 0)
        {
            cout << getNumeral(students[i].source) << " variant";
        }
        else
        {
            if (i <= 3)
                cout << getNumeral(i) << " variant";
            else
                cout << "should be sent down";
        }
        cout << endl;
    }
}

void dereference(Student *students, int number)
{
    bool process = true;
    while (process)
    {
        process = false;

        for (int i = 1; i < number; i++)
        {
            if (students[i].source > 3)
            {
                students[i].source = students[students[i].source].source;
                process = true;
            }
        }
    }
}

int main()
{
    int numStudents = inputNumber("the number of students");

    Student *students = new Student[numStudents + 1]; // + 1 because the first student has code 1
    initializeStudents(students, numStudents);

    inputCopies(students);
    dereference(students, numStudents);
    printStudents(students, numStudents);

    delete[] students;

    return 0;
}
