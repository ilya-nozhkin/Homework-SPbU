package s2_144.nozhkin.test1_1;

public class ListPriorityQueue<T> implements PriorityQueue<T> {
    /** head of the sorted list */
    Node head = null;

    @Override
    public void enqueue(T value, int priority) {
        insert(new Node(value, priority));
    }

    @Override
    public T dequeue() throws EmptyQueueException {
        if (head == null) {
            throw new EmptyQueueException();
        }

        T value = head.value;
        head = head.next;
        return value;
    }

    /**
     * Inserts node in list so that all nodes before have higher priority and all nodes after have lower priority
     * @param node node
     */
    private void insert(Node node) {
        if (head == null) {
            head = node;
            return;
        }

        if (node.priority > head.priority) {
            node.next = head;
            head = node;
            return;
        }

        Node cursor = head;
        while (cursor.next != null && cursor.next.priority > node.priority) {
            cursor = cursor.next;
        }

        node.next = cursor.next;
        cursor.next = node;

    }

    /** element of list sorted by priority */
    private class Node {
        /** value that stored in the list */
        private T value;

        /** priority of the value */
        private int priority;

        /** reference to the next node in list */
        private Node next = null;

        /**
         * constructor for fast initialization.
         * stores parameters to local fields
         *
         * @param value value that will be stored to local field
         * @param priority priority that will be stored to local field
         */
        public Node(T value, int priority) {
            this.value = value;
            this.priority = priority;
        }
    }
}
