import org.junit.Assert;
import org.junit.Test;
import s2_144.nozhkin.task2_2.DoublyLinkedList;
import s2_144.nozhkin.task2_2.List;
import s2_144.nozhkin.task2_2.SinglyLinkedList;

import java.util.Iterator;
import java.util.ListIterator;

public class ListTest {
    private static final int INTEGER_TEST_ELEMENTS_NUMBER = 1024;

    private static void fillList(List<Integer> list) {
        for (int i = 0; i < INTEGER_TEST_ELEMENTS_NUMBER; i++)
            list.add(i);
    }

    private static void checkAndEraseElements(Iterator<Integer> iterator, int start, int end, int offset) {
        for (int i = 0; i < start; i++)
            Assert.assertTrue(iterator.next() == i);

        for (int i = start; i <= end; i++) {
            int expectedValue = i + offset;
            Assert.assertTrue(iterator.next() == expectedValue);
            iterator.remove();
        }
    }

    private static void forwardTest(List<Integer> list) {
        fillList(list);

        ListIterator<Integer> iterator = list.iterator();

        int eraseStart = INTEGER_TEST_ELEMENTS_NUMBER / 4;
        int eraseEnd = INTEGER_TEST_ELEMENTS_NUMBER / 2;

        checkAndEraseElements(iterator, eraseStart, eraseEnd, 0);

        iterator = list.iterator(); //rewind

        checkAndEraseElements(iterator, 0, eraseStart - 1, 0);
        checkAndEraseElements(iterator, 0, INTEGER_TEST_ELEMENTS_NUMBER - 1 - eraseEnd - 1, eraseEnd + 1);

        Assert.assertTrue(list.isEmpty());
    }

    private static void backwardTest(List<Integer> list) {
        fillList(list);

        ListIterator<Integer> iterator = list.iterator();

        while (iterator.hasNext())
            iterator.next();

        int i = INTEGER_TEST_ELEMENTS_NUMBER - 2;
        while (iterator.hasPrevious()) {
            int value = iterator.previous();
            Assert.assertTrue(value == i);
            if (i % 2 == 0)
                iterator.remove();
            i--;
        }

        i = 0;
        while (iterator.hasNext()) {
            int value = iterator.next();
            Assert.assertTrue(value == i + 1);
            iterator.add(i);
            i += 2;
        }

        iterator = list.iterator(); //final check
        for (i = 0; i < INTEGER_TEST_ELEMENTS_NUMBER; i++) {
            int value = iterator.next();
            Assert.assertTrue(value == i);
        }
    }

    @Test
    public void singlyLinkedListTest() {
        SinglyLinkedList<Integer> list = new SinglyLinkedList<>();
        forwardTest(list);
    }

    @Test
    public void doublyLinkedListForwardTest() {
        DoublyLinkedList<Integer> list = new DoublyLinkedList<>();
        forwardTest(list);
    }

    @Test
    public void doublyLinkedListBackwardTest() {
        DoublyLinkedList<Integer> list = new DoublyLinkedList<>();
        backwardTest(list);
    }
}
