package s2_144.nozhkin.task2_1;

public class ListStack<T> implements Stack<T> {
    private Element head = null;

    @Override
    public void push(T value) {
        head = new Element(value, head);
    }

    @Override
    public T pop() {
        if (head == null)
            throw new StackIsEmptyException();

        T value = head.value;
        head = head.next;

        return value;
    }

    @Override
    public boolean isEmpty() {
        return head == null;
    }

    private class Element {
        private T value = null;
        private Element next = null;

        public Element(T value, Element next) {
            this.value = value;
            this.next = next;
        }
    }
}
