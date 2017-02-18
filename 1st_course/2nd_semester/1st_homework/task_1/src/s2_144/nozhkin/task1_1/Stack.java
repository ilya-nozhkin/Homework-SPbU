package s2_144.nozhkin.task1_1;

public class Stack<T> {
    public Stack() {
        head = null;
    }

    public void push(T value) {
        head = new Element(value, head);
    }

    public T pop() {
        if (head == null)
            throw new StackIsEmptyException();

        T value = head.getValue();
        head = head.getNext();

        return value;
    }

    public boolean isEmpty() {
        return head == null;
    }

    public static class StackIsEmptyException extends RuntimeException {
        public StackIsEmptyException() {
            super("'pop' has been called but stack is empty");
        }
    }

    private class Element {
        public Element(T value, Element next) {
            this.value = value;
            this.next = next;
        }

        public T getValue() {
            return value;
        }

        public Element getNext() {
            return next;
        }

        private T value;
        private Element next;
    }

    private Element head;
}
