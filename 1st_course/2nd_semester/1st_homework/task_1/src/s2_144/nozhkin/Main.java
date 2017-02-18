package s2_144.nozhkin;

public class Main {

    public static void main(String[] args) {
        System.out.println(integerTest() ? "Integer test has been passed" : "Integer test hasn't been passed");
        System.out.println(stringTest() ? "String test has been passed" : "String test hasn't been passed");
    }

    public static boolean integerTest() {
        Stack<Integer> stack = new Stack<Integer>();

        for (int i = 0; i < testElementsNumber; i++)
            stack.push(i);

        for (int i = testElementsNumber - 1; i >= 0; i--)
            if (stack.pop() != i)
                return false;

        if (!stack.isEmpty())
            return false;

        //let's check error handling system
        try {
            stack.pop();
        } catch (Stack.StackIsEmptyException e) {
            return true; //throwing an exception is normal behavior in that case because stack should be empty
        }

        return false; //else stack is not empty
    }

    public static boolean stringTest() {
        Stack<String> stack = new Stack<String>();

        for (int i = 0; i < testElementsNumber; i++)
            stack.push(Integer.toString(i));

        for (int i = testElementsNumber - 1; i >= 0; i--)
            if (!stack.pop().equals(Integer.toString(i)))
                return false;

        return true;
    }

    private static final int testElementsNumber = 128;
}
