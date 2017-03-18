package s2_144.nozhkin.task4_1;

import java.util.Iterator;
import java.util.ListIterator;

public class UniqueList<T> implements List<T> {
    LinkedList<T> list = null;

    public UniqueList() {
        list = new LinkedList<>();
    }

    public boolean exists(T value) {
        for (T data : list) {
            if (data.equals(value)) {
                return true;
            }
        }

        return false;
    }

    public void add(T value) throws AlreadyExistsException {
        if (exists(value))
            throw new AlreadyExistsException(value.toString());

        list.add(value);
    }

    @Override
    public boolean isEmpty() {
        return list.isEmpty();
    }

    public void remove(T value) throws NoSuchElementException {
        ListIterator<T> iterator = list.iterator();
        while (iterator.hasNext()) {
            T data = iterator.next();
            if (data.equals(value)) {
                iterator.remove();
                return;
            }
        }

        throw new NoSuchElementException(value.toString());
    }

    @Override
    public Iterator<T> iterator() {
        return list.iterator();
    }

    public static class AlreadyExistsException extends RuntimeException {
        public AlreadyExistsException(String value) {
            super("Element " + value + " already exists");
        }
    }

    public static class NoSuchElementException extends Exception {
        public NoSuchElementException(String value) {
            super("LinkedList doesn't contain element " + value);
        }
    }
}
