import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import s2_144.nozhkin.task3_1.BubbleSorter;
import s2_144.nozhkin.task3_1.QuickSorter;
import s2_144.nozhkin.task3_1.SelectionSorter;
import s2_144.nozhkin.task3_1.Sorter;

import java.util.Comparator;
import java.util.Random;

public class SortersTest {
    private static final int TEST_SET_SIZE = 4096;

    private Random generator;

    @Before
    public void initializeResources() {
        generator = new Random();
    }

    private Integer[] generateRandomIntegerSet(int size) {
        Integer[] set = new Integer[TEST_SET_SIZE];
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            set[i] = generator.nextInt();
        }

        return set;
    }

    private Vector[] generateRandomVectorSet(int size) {
        Vector[] set = new Vector[TEST_SET_SIZE];
        for (int i = 0; i < TEST_SET_SIZE; i++) {
            set[i] = new Vector();
            set[i].x = generator.nextDouble();
            set[i].y = generator.nextDouble();
        }

        return set;
    }

    private void ascendingIntegerTest(Sorter<Integer> sorter) {
        //we can not use pre-generated arrays because it is reordered by sorter each time
        Integer[] testSet = generateRandomIntegerSet(TEST_SET_SIZE);
        sorter.sort(testSet);

        for (int i = 0; i < TEST_SET_SIZE - 1; i++)
            Assert.assertTrue(testSet[i] < testSet[i + 1]);
    }

    private void descendingIntegerTest(Sorter<Integer> sorter) {
        //we can not use pre-generated arrays because it is reordered by sorter each time
        Integer[] testSet = generateRandomIntegerSet(TEST_SET_SIZE);
        sorter.sort(testSet);

        for (int i = 0; i < TEST_SET_SIZE - 1; i++)
            Assert.assertTrue(testSet[i] > testSet[i + 1]);
    }

    private void vectorTest(Sorter<Vector> sorter) {
        //we can not use pre-generated arrays because it is reordered by sorter each time
        Vector[] testSet = generateRandomVectorSet(TEST_SET_SIZE);
        sorter.sort(testSet);

        for (int i = 0; i < TEST_SET_SIZE - 1; i++)
            Assert.assertTrue(testSet[i].length() < testSet[i + 1].length());
    }

    @Test
    public void bubbleSorterTest() {
        Sorter<Integer> integerSorter = new BubbleSorter<>(new AscendingIntegerComparator());
        ascendingIntegerTest(integerSorter);

        integerSorter = new BubbleSorter<>(new DescendingIntegerComparator());
        descendingIntegerTest(integerSorter);

        Sorter<Vector> vectorSorter = new BubbleSorter<>(new RadialComparator());
        vectorTest(vectorSorter);
    }

    @Test
    public void selectionSorterTest() {
        Sorter<Integer> integerSorter = new SelectionSorter<>(new AscendingIntegerComparator());
        ascendingIntegerTest(integerSorter);

        integerSorter = new SelectionSorter<>(new DescendingIntegerComparator());
        descendingIntegerTest(integerSorter);

        Sorter<Vector> vectorSorter = new SelectionSorter<>(new RadialComparator());
        vectorTest(vectorSorter);
    }

    @Test
    public void quickSorterTest() {
        Sorter<Integer> integerSorter = new QuickSorter<>(new AscendingIntegerComparator());
        ascendingIntegerTest(integerSorter);

        integerSorter = new QuickSorter<>(new DescendingIntegerComparator());
        descendingIntegerTest(integerSorter);

        Sorter<Vector> vectorSorter = new QuickSorter<>(new RadialComparator());
        vectorTest(vectorSorter);
    }

    private class Vector {
        public double x;
        public double y;

        public double length() {
            return Math.sqrt(x * x + y * y);
        }
    }

    private class RadialComparator implements Comparator<Vector> {
        @Override
        public int compare(Vector left, Vector right) {
            double leftLength = left.length();
            double rightLength = right.length();
            return leftLength < rightLength ? -1 : ((leftLength > rightLength) ? 1 : 0);
        }
    }

    private class AscendingIntegerComparator implements Comparator<Integer> {
        @Override
        public int compare(Integer left, Integer right) {
            return left < right ? -1 : ((left > right) ? 1 : 0);
        }
    }

    private class DescendingIntegerComparator implements Comparator<Integer> {
        @Override
        public int compare(Integer left, Integer right) {
            return left > right ? -1 : ((left < right) ? 1 : 0);
        }
    }
}
