package s2_144.nozhkin.task8_2;

import java.util.Comparator;

public class SingleThreadQuickSorter<T> extends QuickSorter<T> {
    /**
     * constructor that matching the constructor of the superclass
     *
     * @param comparator comparator that can compare elements of type T
     */
    public SingleThreadQuickSorter(Comparator<T> comparator) {
        super(comparator);
    }

    @Override
    public boolean sort(T[] array) {
        singleThreadSort(array, 0, array.length - 1);

        return true;
    }

    /**
     * performs partitioning and calls itself recursively for left and right parts
     *
     * @param array array that should be sorted
     * @param first left element
     * @param last right element
     */
    private void singleThreadSort(T[] array, int first, int last) {
        int cursor = partition(array, first, last);

        if (cursor - 1 - first > 0)
            singleThreadSort(array, first, cursor - 1);
        if (last - cursor > 0)
            singleThreadSort(array, cursor, last);
    }
}
