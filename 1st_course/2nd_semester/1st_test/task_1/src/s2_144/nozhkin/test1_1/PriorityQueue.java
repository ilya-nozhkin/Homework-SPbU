package s2_144.nozhkin.test1_1;

/** Abstract type of data that takes values and gives them back sorted by priority */
public interface PriorityQueue<T> {
    /**
     * put value with some priority
     *
     * @param value value
     * @param priority priority
     */
    void enqueue(T value, int priority);

    /**
     * returns element with highest priority
     *
     * @return value with highest priority
     * @throws EmptyQueueException when queue is empty
     */
    T dequeue() throws EmptyQueueException;

    /** exception that is thrown when queue is empty and dequeue is called */
    class EmptyQueueException extends Exception {
        /** default constructor that initializes this exception with message telling about problem */
        public EmptyQueueException() {
            super("Method dequeue has been called but queue is empty");
        }
    }
}
