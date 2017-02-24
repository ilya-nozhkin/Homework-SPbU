package s2_144.nozhkin.task1_2;

import java.util.ListIterator;

public class List<T> implements Iterable<T> {
    private Element head = null;
    private Element tail = null;

    public void add(T value) {
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
    public ListIterator<T> iterator() {
        return new MyListIterator();
    }

    private class Element {
        private T value = null;
        private Element next = null;

        public Element(T value, Element next) {
            this.value = value;
            this.next = next;
        }

        public void setValue(T value) {
            this.value = value;
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

    private class MyListIterator implements ListIterator<T> {
        Element previousElement = null;
        Element currentElement = null;
        Element nextElement = head;
        int index = -1;

        @Override
        public boolean hasNext() {
            return nextElement != null;
        }

        @Override
        public T next() {
            if (nextElement == null)
                return null;

            previousElement = currentElement;
            currentElement  = nextElement;
            nextElement     = nextElement.getNext();

            index++;

            return currentElement.getValue();
        }

        @Override
        public boolean hasPrevious() {
            return false;
        }

        @Override
        public T previous() {
            return null;
        }

        @Override
        public int nextIndex() {
            return index + 1;
        }

        @Override
        public int previousIndex() {
            return index - 1;
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

            index--;
        }

        @Override
        public void set(T value) {
            currentElement.setValue(value);
        }

        public void add(T value) {
            if (currentElement == previousElement)
                throw new AddBeforeNextException();

            if (previousElement == null) {
                head = new Element(value, head);
            } else {
                Element newElement = new Element(value, currentElement);
                previousElement.setNext(newElement);
                previousElement = newElement;
            }

            index++;
        }
    }

    public static class AddBeforeNextException extends RuntimeException {
        public AddBeforeNextException() {
            super("You've tried to add element before calling 'next'");
        }
    }

    public static class DoubleRemoveException extends RuntimeException {
        public DoubleRemoveException() {
            super("Method 'remove' has been called twice after last 'next' method calling");
        }
    }
}
