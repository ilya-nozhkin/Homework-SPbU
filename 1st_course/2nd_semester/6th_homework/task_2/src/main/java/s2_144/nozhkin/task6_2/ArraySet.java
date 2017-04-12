package s2_144.nozhkin.task6_2;

import java.util.*;

/** class that implements set using ArrayList */
public class ArraySet<T> implements Set<T> {
    /** collection that stores elments of the set */
    private ArrayList<T> array = new ArrayList<>();

    public int size() {
        return array.size();
    }

    public boolean isEmpty() {
        return array.isEmpty();
    }

    public boolean contains(Object value) {
        return array.contains(value);
    }

    public Iterator<T> iterator() {
        return array.iterator();
    }

    public Object[] toArray() {
        return array.toArray();
    }

    public <E> E[] toArray(E[] array) {
        return this.array.toArray(array);
    }

    public boolean add(T value) {
        if (contains(value)) {
            return false;
        }
        return array.add(value);
    }

    public boolean remove(Object value) {
        return array.remove(value);
    }

    public boolean containsAll(Collection<?> collection) {
        return array.containsAll(collection);
    }

    public boolean addAll(Collection<? extends T> collection) {
        boolean status = false;
        for (T value : collection) {
            if (add(value)) {
                status = true;
            }
        }
        return status;
    }

    public boolean retainAll(Collection<?> collection) {
        return array.retainAll(collection);
    }

    public boolean removeAll(Collection<?> collection) {
        return array.removeAll(collection);
    }

    public void clear() {
        array.clear();
    }
}
