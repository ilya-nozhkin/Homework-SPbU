package s2_144.nozhkin.task3_1;

import java.util.Comparator;

public abstract class ComparisonSorter<T> implements Sorter<T> {
    protected Comparator<T> comparator;

    public ComparisonSorter(Comparator<T> comparator) {
        this.comparator = comparator;
    }
}
