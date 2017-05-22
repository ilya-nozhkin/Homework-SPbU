package s2_144.nozhkin.task8_2;

import java.util.Comparator;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class MultiThreadQuickSorter<T> extends QuickSorter<T> {
    /** thread executor that is used to launch threads */
    private ExecutorService executor;

    /** number of threads that can be launched at the same time */
    private int maxThreads;

    /** counter that counts a number of launched threads */
    private Counter counter = new Counter();

    /**
     * constructor that stores a comparator and initializes thread pool
     *
     * @param comparator
     * @param maxThreads
     */
    public MultiThreadQuickSorter(Comparator<T> comparator, int maxThreads) {
        super(comparator);

        this.maxThreads = maxThreads;
        executor = Executors.newFixedThreadPool(maxThreads);
    }

    @Override
    public boolean sort(T[] array) {
        if (counter.threads > 0) {
            return false;
        }

        counter.threads = 1;
        executor.submit(() -> multiThreadSort(array, 0, array.length - 1, true));

        boolean process = true;
        while(process) {
            synchronized (counter) {
                if (counter.threads == 0) {
                    process = false;
                } else {
                    try {
                        counter.wait();
                        if (counter.threads == 0) {
                            process = false;
                        }
                    } catch (InterruptedException e) {
                        return false;
                    }
                }
            }
        }

        return true;
    }

    /**
     * launches a next quicksort pass in a new thread if current number
     * of threads is less than max and in current thread otherwise
     *
     * @param array array that should be sorted
     * @param first left element
     * @param last right element
     */
    private void nextPass(T[] array, int first, int last) {
        if (last > first) {
            boolean parallel = false;
            synchronized (counter) {
                if (counter.threads < maxThreads) {
                    parallel = true;
                    counter.threads++;
                }
            }

            if (parallel) {
                executor.submit(() -> multiThreadSort(array, first, last, true));
            } else {
                multiThreadSort(array, first, last, false);
            }
        }
    }

    /**
     * performs partitioning and calls itself recursively for left and right parts
     *
     * @param array array that should be sorted
     * @param first left element
     * @param last right element
     * @param parallel true if this function is called in new thread and false if in old thread
     */
    private void multiThreadSort(T[] array, int first, int last, boolean parallel) {
        int cursor = partition(array, first, last);

        nextPass(array, first, cursor - 1);
        nextPass(array, cursor, last);

        if (parallel) {
            synchronized (counter) {
                counter.threads--;
                counter.notify();
            }
        }
    }

    /** is used for thread synchronizing, stores a number of launched threads */
    private class Counter {
        int threads;
    }
}
