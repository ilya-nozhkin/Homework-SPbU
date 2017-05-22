import org.junit.Before;
import org.junit.Test;
import s2_144.nozhkin.task8_2.MultiThreadQuickSorter;
import s2_144.nozhkin.task8_2.QuickSorter;
import s2_144.nozhkin.task8_2.SingleThreadQuickSorter;

import java.util.Comparator;
import java.util.Random;

import static org.junit.Assert.*;

public class QuickSorterTest {
    private static final int CORRECTNESS_TEST_SIZE = 16*1024;
    private static final int SPEED_TEST_SIZE = 2*1024*1024;
    private static final int THREADS = 9;

    /** random numbers generator */
    private Random generator;

    /** initialize random numbers generator */
    @Before
    public void initializeResources() {
        generator = new Random();
    }

    /**
     * generates an array of random integers
     *
     * @param size size of array
     * @return array of random numbers
     */
    private Integer[] generateRandomIntegerSet(int size) {
        Integer[] set = new Integer[size];
        for (int i = 0; i < size; i++) {
            set[i] = generator.nextInt();
        }

        return set;
    }

    /** tests the order of elements after single-threaded realization of quicksort */
    @Test
    public void singleThreadCorrectnessTest() {
        QuickSorter<Integer> sorter = new SingleThreadQuickSorter<>(Comparator.comparing(Integer::intValue));

        Integer[] testSet = generateRandomIntegerSet(CORRECTNESS_TEST_SIZE);
        sorter.sort(testSet);

        for (int i = 0; i < CORRECTNESS_TEST_SIZE - 1; i++)
            assertTrue(testSet[i] <= testSet[i + 1]);
    }

    /** tests the order of elements after multi-threaded realization of quicksort */
    @Test
    public void multiThreadCorrectnessTest() {
        QuickSorter<Integer> sorter = new MultiThreadQuickSorter<>(
                Comparator.comparing(Integer::intValue), THREADS);

        Integer[] testSet = generateRandomIntegerSet(CORRECTNESS_TEST_SIZE);
        sorter.sort(testSet);

        for (int i = 0; i < CORRECTNESS_TEST_SIZE - 1; i++)
            assertTrue(testSet[i] <= testSet[i + 1]);
    }

    /** tests that multi-threaded quicksort is faster than single-threaded */
    @Test
    public void speedTest() {
        Integer[] testSet = generateRandomIntegerSet(SPEED_TEST_SIZE);

        Integer[] singleThreadData = testSet.clone();
        Integer[] multiThreadData = testSet.clone();

        QuickSorter<Integer> singleThreadSorter = new SingleThreadQuickSorter<>(Comparator.comparing(Integer::intValue));

        QuickSorter<Integer> multiThreadSorter = new MultiThreadQuickSorter<>(
                Comparator.comparing(Integer::intValue), THREADS);

        long start = System.currentTimeMillis();
        singleThreadSorter.sort(singleThreadData);
        long singleThreadTime = System.currentTimeMillis() - start;

        start = System.currentTimeMillis();
        multiThreadSorter.sort(multiThreadData);
        long multiThreadTime = System.currentTimeMillis() - start;

        System.out.println("Single-thread time: " + singleThreadTime + " ms");
        System.out.println("Multi-thread time: " + multiThreadTime + " ms");
        assertTrue(multiThreadTime < singleThreadTime);
    }
}
