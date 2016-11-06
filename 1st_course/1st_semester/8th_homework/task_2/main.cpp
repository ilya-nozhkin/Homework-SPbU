#include <iostream>
#include "myString.h"

using namespace std;

int main()
{
    String *first = createString("first string");
    String *second = createString();

    cout << "The first string is \"" << castToChars(first) << "\"" << endl;
    cout << "The second string is \"" << castToChars(second) << "\"" << endl;
    cout << endl;

    cout << "Length of the first string = " << length(first) << endl;
    cout << "Length of the second string = " << length(second) << endl;
    cout << endl;

    cout << "Is the first string empty? " << (isEmpty(first) ? "yes" : "no") << endl;
    cout << "Is the second string empty? " << (isEmpty(second) ? "yes" : "no") << endl;
    cout << endl;

    String *third = clone(first);

    cout << "The third string is a clone of the first and it is \"" << castToChars(third) << "\"" << endl;
    cout << "Are first and third strings equal? " << (equals(first, third) ? "yes" : "no") << endl;
    cout << "And second and third? " << (equals(second, third) ? "yes" : "no") << endl;
    cout << endl;

    cout << "Let's try to concatenate second and third strings. " << endl;
    concatenate(second, third);
    cout << "Now, second string is \"" << castToChars(second) << "\"" << endl;
    cout << endl;

    cout << "Also I want to extract some substring from first string" << endl;
    cout << "Let's try to get 5 characters which begin from 6'th" << endl;
    String *fourth = substring(first, 6, 5);
    cout << "This substring is \"" << castToChars(fourth) << "\"";
    cout << endl;

    cout << "Next, we'll concatenate second and first and then third and first" << endl;
    concatenate(second, first);
    concatenate(third, first);
    cout << endl;

    cout << "I hope second and third strings are equal now, let's check it" << endl;
    cout << "The second string is \"" << castToChars(second) << "\"" << endl;
    cout << "The third string is \"" << castToChars(third) << "\"" << endl;
    bool status = equals(second, third);
    cout << (status ? "It is ok :)" : "Uhmmm... Something went wrong :(") << endl;
    cout << endl;

    cout << (status ? "Then" : "Anyway");
    cout << " let's delete all of our strings." << endl;

    deleteString(fourth);
    deleteString(third);
    deleteString(second);
    deleteString(first);

    return 0;
}
