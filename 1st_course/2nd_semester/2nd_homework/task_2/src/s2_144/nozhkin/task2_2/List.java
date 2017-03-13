package s2_144.nozhkin.task2_2;

import java.util.ListIterator;

public interface List<T> extends Iterable<T> {
    void add(T value);
    boolean isEmpty();

    @Override
    ListIterator<T> iterator();
}
