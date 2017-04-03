package s2_144.nozhkin.task4_2;

public interface List<T> extends Iterable<T> {
    void add(T value);
    boolean isEmpty();
    int size();
}