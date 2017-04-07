import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import s2_144.nozhkin.test1_1.ListPriorityQueue;
import s2_144.nozhkin.test1_1.PriorityQueue;

import java.util.Arrays;
import java.util.Random;

import static org.junit.Assert.*;

public class PriorityQueueTest {
    /** size of test set in the order test */
    private static int TEST_SET_SIZE = 1024;

    /** generator that is used for filling a test set */
    private Random generator = null;

    /** initializes random numbers generator */
    @Before
    public void initialize() {
        generator = new Random();
    }

    /** makes reference to random numbers generator null */
    @After
    public void terminate() {
        generator = null;
    }

    /**
     * compares order of returned values with values sorted by priority
     *
     * @throws PriorityQueue.EmptyQueueException shouldn't be thrown!
     */
    @Test
    public void orderTest() throws PriorityQueue.EmptyQueueException {
        PriorityQueue<Integer> queue = new ListPriorityQueue<>();

        Pair[] testSet = createRandomSet(TEST_SET_SIZE);

        for (Pair pair : testSet) {
            queue.enqueue(pair.value, pair.priority);
        }

        Arrays.sort(testSet);

        for (Pair pair : testSet) {
            int value = queue.dequeue();
            assertTrue(value == pair.value);
        }
    }

    /**
     * checks that queue throws exception when you try to dequeue value from empty queue
     *
     * @throws PriorityQueue.EmptyQueueException should be thrown because there are two dequeues and one enqueue
     */
    @Test(expected = PriorityQueue.EmptyQueueException.class)
    public void dequeueFromEmptyQueueTest() throws PriorityQueue.EmptyQueueException {
        PriorityQueue<Integer> queue = new ListPriorityQueue<>();

        int value = generator.nextInt();
        queue.enqueue(value, generator.nextInt());
        queue.dequeue();

        queue.dequeue();
    }

    /**
     * creates array that is filled with random number
     *
     * @param size size of that array
     * @return array of random numbers
     */
    private Pair[] createRandomSet(int size) {
        Pair set[] = new Pair[size];
        for (int i = 0; i < size; i++)
            set[i] = new Pair(generator.nextInt(), generator.nextInt());
        return set;
    }

    /** pair of two values - value and priority, array of such pairs can be sorted in descending order */
    private class Pair implements Comparable<Pair> {
        /** value */
        int value;

        /** priority */
        int priority;

        /**
         * constructor for fast initialization.
         * stores parameters to local fields
         *
         * @param value value that will be stored to local field
         * @param priority priority that will be stored to local field
         */
        public Pair(int value, int priority) {
            this.value = value;
            this.priority = priority;
        }

        @Override
        public int compareTo(Pair o) {
            return priority > o.priority ? -1 : priority < o.priority ? 1 : 0;
        }
    }
}
