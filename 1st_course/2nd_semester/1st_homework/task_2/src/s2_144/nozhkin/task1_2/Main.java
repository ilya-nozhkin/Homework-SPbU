package s2_144.nozhkin.task1_2;

import java.util.Iterator;
import java.util.ListIterator;

public class Main {
    private static final int testElementsNumber = 128;

    public static void main(String[] args) {
        System.out.println(integerTest() ? "Integer test has been passed" : "Integer test hasn't been passed");
        System.out.println(stringTest() ? "String test has been passed" : "String test hasn't been passed");
    }

    private static void fillList(List<Integer> list) {
        for (int i = 0; i < testElementsNumber; i++)
            list.add(i);
    }

    private static boolean checkAndEraseElements(Iterator<Integer> iterator, int start, int end, int offset) {
        for (int i = 0; i < start; i++)
            if (iterator.next() != i)
                return false;

        for (int i = start; i <= end; i++) {
            int expectedValue = i + offset;
            if (iterator.next() != expectedValue)
                return false;
            iterator.remove();
        }

        return true;
    }

    private static boolean integerTest() {
        List<Integer> list = new List<Integer>();

        fillList(list);

        ListIterator<Integer> iterator = list.iterator();

        int eraseStart = testElementsNumber / 4;
        int eraseEnd = testElementsNumber / 2;

        if (!checkAndEraseElements(iterator, eraseStart, eraseEnd, 0))
            return false;

        iterator = list.iterator(); //rewind

        if (!checkAndEraseElements(iterator, 0, eraseStart - 1, 0))
            return false;

        if (!checkAndEraseElements(iterator, 0, testElementsNumber - 1 - eraseEnd - 1, eraseEnd + 1))
            return false;

        if (!list.isEmpty())
            return false;

        return true;
    }

    private static boolean stringTest() {
        List<String> list = new List<String>();

        for (int i = 0; i < testElementsNumber; i++)
            list.add(Integer.toString(i * 2 + 1));

        ListIterator<String> iterator = list.iterator();

        for (int i = 0; i < testElementsNumber; i++) {
            String expectedValue = Integer.toString(i * 2 + 1);
            if (!iterator.next().equals(expectedValue))
                return false;

            iterator.add(Integer.toString(i * 2));
        }

        iterator = list.iterator(); //rewind

        for (int i = 0; i < testElementsNumber * 2; i++) {
            String expectedValue = Integer.toString(i);
            if (!iterator.next().equals(expectedValue))
                return false;
        }

        return true;
    }
}
