import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.Assert;

import s2_144.nozhkin.task2_1.ListStack;
import s2_144.nozhkin.task2_1.Stack;
import s2_144.nozhkin.task2_1.VectorStack;

public class StackTest {
    private static final int TEST_ELEMENTS_NUMBER = 1000000;
    private String[] stringTestData = null;

    @Before
    public void initializeTestData() {
        stringTestData = new String[TEST_ELEMENTS_NUMBER];
    }

    @After
    public void freeTestData() {
        stringTestData = null;
    }

    public void integerTest(Stack<Integer> stack) {
        for (int i = 0; i < TEST_ELEMENTS_NUMBER; i++)
            stack.push(i);

        for (int i = TEST_ELEMENTS_NUMBER - 1; i >= 0; i--)
            Assert.assertTrue(stack.pop() == i);

        Assert.assertTrue(stack.isEmpty());

        stack.pop();
    }

    public void stringTest(Stack<String> stack) {
        for (int i = 0; i < TEST_ELEMENTS_NUMBER; i++)
            stringTestData[i] = Integer.toString(i);

        for (int i = 0; i < TEST_ELEMENTS_NUMBER; i++)
            stack.push(stringTestData[i]);

        for (int i = TEST_ELEMENTS_NUMBER - 1; i >= 0; i--)
            Assert.assertTrue(stack.pop().equals(stringTestData[i]));
    }

    @Test(expected = Stack.StackIsEmptyException.class)
    public void integerListStackTest() {
        ListStack<Integer> stack = new ListStack<>();
        integerTest(stack);
    }

    @Test(expected = Stack.StackIsEmptyException.class)
    public void integerVectorStackTest() {
        VectorStack<Integer> stack = new VectorStack<>();
        integerTest(stack);
    }

    @Test
    public void stringListStackTest() {
        ListStack<String> stack = new ListStack<>();
        stringTest(stack);
    }

    @Test
    public void stringVectorStackTest() {
        VectorStack<String> stack = new VectorStack<>();
        stringTest(stack);
    }
}