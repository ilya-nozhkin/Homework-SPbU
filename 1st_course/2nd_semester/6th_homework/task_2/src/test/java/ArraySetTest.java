import org.junit.Test;
import s2_144.nozhkin.task6_2.ArraySet;

import java.util.ArrayList;
import java.util.Random;
import java.util.Set;

import static org.junit.Assert.*;

/** test that checks behavior of class ArraySet */
public class ArraySetTest {
    private static final int TEST_SET_SIZE = 257;

    /** used for random numbers generation */
    Random generator = new Random();

    /**
     * fills received set with random numbers
     *
     * @param set set that should be filled
     */
    private void fillWithRandomNumbers(Set<Integer> set) {
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            set.add(generator.nextInt());
        }
    }

    /**
     * fills received array with random numbers
     *
     * @param array array that should be filled
     */
    private void fillWithRandomNumbers(ArrayList<Integer> array) {
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            array.add(generator.nextInt());
        }
    }

    /**
     * fills received array and set with same random numbers
     *
     * @param set set that should be filled
     * @param array array that should be filled
     */
    private void fillWithRandomNumbers(Set<Integer> set, ArrayList<Integer> array) {
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            int value = generator.nextInt();
            array.add(value);
            set.add(value);
        }
    }

    /** test that checks method size */
    @Test
    public void sizeTest() {
        ArraySet<Integer> set = new ArraySet<>();

        fillWithRandomNumbers(set);

        assertTrue(set.size() == TEST_SET_SIZE);
    }

    /** test that checks method isEmpty */
    @Test
    public void isEmptyTest() {
        ArraySet<Integer> set = new ArraySet<>();

        assertTrue(set.isEmpty());

        int value = generator.nextInt();
        set.add(value);
        set.remove(value);

        assertTrue(set.isEmpty());
    }

    /** test that checks method contains */
    @Test
    public void containsTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(set, testSet);

        for (Integer value : testSet) {
            assertTrue(set.contains(value));
        }
    }

    /** test that checks method iterator */
    @Test
    public void iteratorTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(set, testSet);

        //method iterator called implicit
        for (Integer value : set) {
            assertTrue(testSet.contains(value));
        }
    }

    /** test that checks method toArray */
    @Test
    public void toArrayTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(set, testSet);

        Integer[] array = set.toArray(new Integer[set.size()]);

        for (Integer value : array) {
            assertTrue(testSet.contains(value));
        }
    }

    /** test that checks method add */
    @Test
    public void addTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        int previousSize = set.size();
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            int value = generator.nextInt();
            testSet.add(value);
            set.add(value);

            assertTrue(set.size() == previousSize + 1);
            assertTrue(set.contains(value));

            previousSize = set.size();
        }

        for (Integer value : testSet) {
            assertFalse(set.add(value));
            assertTrue(previousSize == set.size());
        }
    }

    /** test that checks method remove */
    @Test
    public void removeTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(set, testSet);

        int previousSize = set.size();
        for (Integer value : testSet) {
            set.remove(value);

            assertTrue(!set.contains(value));
            assertTrue(set.size() == previousSize - 1);

            previousSize = set.size();
        }
    }

    /** test that checks method containsAll */
    @Test
    public void containsAllTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(set, testSet);

        assertTrue(set.containsAll(testSet));
    }

    /** test that checks method addAll */
    @Test
    public void addAllTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(testSet);

        assertTrue(set.addAll(testSet));
        assertFalse(set.addAll(testSet));
        assertTrue(set.containsAll(testSet));
    }

    /** test that checks method retainAll */
    @Test
    public void retainAllTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            testSet.add(i);
        }

        set.addAll(testSet);

        ArrayList<Integer> testSetCopy = new ArrayList<>(testSet);

        int half = TEST_SET_SIZE / 2;
        for (int i = half; i < TEST_SET_SIZE; i++) {
            testSetCopy.remove(half);
        }

        set.retainAll(testSetCopy);

        assertTrue(set.containsAll(testSetCopy));

        for (int i = TEST_SET_SIZE / 2; i < TEST_SET_SIZE; i++) {
            assertFalse(set.contains(testSet.get(i)));
        }
    }

    /** test that checks method removeAll */
    @Test
    public void removeAllTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(testSet);

        set.addAll(testSet);
        set.removeAll(testSet);

        assertTrue(set.size() == 0);
    }

    /** test that checks method clear */
    @Test
    public void clearTest() {
        ArraySet<Integer> set = new ArraySet<>();
        ArrayList<Integer> testSet = new ArrayList(TEST_SET_SIZE);

        fillWithRandomNumbers(testSet);

        set.addAll(testSet);
        set.clear();

        assertTrue(set.size() == 0);
    }
}
