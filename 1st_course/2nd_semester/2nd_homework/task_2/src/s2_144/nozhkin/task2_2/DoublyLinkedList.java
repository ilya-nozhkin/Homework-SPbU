package s2_144.nozhkin.task2_2;

import java.util.ListIterator;

public class DoublyLinkedList<T> implements List<T> {
    private Element head = null;
    private Element tail = null;

    public void add(T value) {
        if (tail == null) {
            head = new Element(value, null, null);
            tail = head;
        } else {
            Element newElement = new Element(value, tail, null);
            tail.next = newElement;
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
        private Element previous = null;

        public Element(T value, Element previous, Element next) {
            this.value = value;
            this.previous = previous;
            this.next = next;
        }
    }

    private class MyListIterator implements ListIterator<T> {
        Element previousElement = null;
        Element currentElement = null;
        Element nextElement = head;
        boolean rightDirection = true;
        int index = -1;

        @Override
        public boolean hasNext() {
            return nextElement != null;
        }

        @Override
        public T next() {
            if (nextElement == null)
                return null;

            if (currentElement != nextElement)
                previousElement = currentElement;
            currentElement = nextElement;
            nextElement = nextElement.next;

            index++;
            rightDirection = true;

            return currentElement.value;
        }

        @Override
        public boolean hasPrevious() {
            return previousElement != null;
        }

        @Override
        public T previous() {
            if (previousElement == null)
                return null;

            if (currentElement != previousElement)
                nextElement = currentElement;
            currentElement = previousElement;
            previousElement = previousElement.previous;

            index--;
            rightDirection = false;

            return currentElement.value;
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
            if (rightDirection) {
                if (previousElement == null)
                    head = nextElement;
                else
                    previousElement.next = nextElement;

                if (nextElement != null)
                    nextElement.previous = previousElement;

                currentElement = previousElement;

                if (nextElement == null)
                    tail = previousElement;

                index--;
            } else {
                if (nextElement == null)
                    tail = previousElement;
                else
                    nextElement.previous = previousElement;

                if (previousElement != null)
                    previousElement.next = nextElement;

                currentElement = nextElement;

                if (previousElement == null)
                    head = nextElement;
            }
        }

        @Override
        public void set(T value) {
            currentElement.value = value;
        }

        @Override
        public void add(T value) {
            if (rightDirection) {
                if (previousElement == null) {
                    head = new Element(value, null, currentElement);
                    currentElement.previous = head;
                } else {
                    Element newElement = new Element(value, previousElement, currentElement);
                    previousElement.next = newElement;
                    currentElement.previous = newElement;
                    previousElement = newElement;
                }
            } else {
                if (nextElement == null) {
                    tail = new Element(value, currentElement, null);
                    currentElement.next = tail;
                } else {
                    Element newElement = new Element(value, currentElement, nextElement);
                    nextElement.previous = newElement;
                    currentElement.next = newElement;
                    nextElement = newElement;
                }
            }
        }
    }
}
