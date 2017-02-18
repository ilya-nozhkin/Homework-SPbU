package s2_144.nozhkin.task1_2;

import java.util.Iterator;

public class List<T> implements Iterable<T> {
    private Element head;
    private Element tail;

    public List() {
        head = null;
        tail = null;
    }

    public void put(T value) {
        if (tail == null) {
            head = new Element(value, null);
            tail = head;
        } else {
            Element newElement = new Element(value, null);
            tail.setNext(newElement);
            tail = newElement;
        }
    }

    public boolean isEmpty() {
        return head == null;
    }

    @Override
    public Iterator<T> iterator() {
        return new ListIterator();
    }

    private class Element {
        private T value;
        private Element next;

        public Element(T value, Element next) {
            this.value = value;
            this.next = next;
        }

        public T getValue() {
            return value;
        }

        public void setNext(Element next) {
            this.next = next;
        }

        public Element getNext() {
            return next;
        }
    }

    private class ListIterator implements Iterator<T> {
        Element previousElement;
        Element currentElement;
        Element nextElement;

        public ListIterator() {
            previousElement = null;
            currentElement = null;
            nextElement = head;
        }

        @Override
        public boolean hasNext() {
            return nextElement != null;
        }

        @Override
        public T next() {
            if (nextElement == null)
                throw new NoNextElementException();

            previousElement = currentElement;
            currentElement  = nextElement;
            nextElement     = nextElement.getNext();

            return currentElement.getValue();
        }

        @Override
        public void remove() {
            if (previousElement == currentElement)
                throw new DoubleRemoveException();

            if (previousElement == null) {
                head = nextElement;
            } else {
                previousElement.setNext(currentElement.getNext());
            }

            currentElement = previousElement;

            if (currentElement == null)
                tail = previousElement;
        }
    }

    public static class NoNextElementException extends RuntimeException {
        public NoNextElementException() {
            super("There is no next element");
        }
    }

    public static class DoubleRemoveException extends RuntimeException {
        public DoubleRemoveException() {
            super("Method 'remove' has been called twice after last 'next' method calling");
        }
    }
}
