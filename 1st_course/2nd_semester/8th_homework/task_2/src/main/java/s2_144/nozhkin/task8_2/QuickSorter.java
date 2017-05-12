package s2_144.nozhkin.task8_2;

import java.util.Comparator;

/** abstract class that provides a sorting interface and realization of partitioning operation */
public abstract class QuickSorter<T> {
    /** comparator which compares array elements */
    protected final Comparator<T> comparator;

    /**
     * constructor that stores the comparator
     *
     * @param comparator comparator that can compare elements of type T
     */
    public QuickSorter(Comparator<T> comparator) {
        this.comparator = comparator;
    }

    /**
     * quicksort partitioning operation
     * this method chooses one element (pivot) and reorder the array so that all elements with values
     * less than the pivot come before the pivot, while all elements with values greater than the pivot come after it
     *
     * @param array that should be parted
     * @param first first element
     * @param last last element
     * @return a position of left iterator
     */
    protected int partition(T[] array, int first, int last) {
        T pivot = array[(first + last) / 2];
        int i = first;
        int j = last;

        while (i <= j) {
            while (i <= j && comparator.compare(array[i], pivot) < 0)
                i++;
            while (j >= i && comparator.compare(array[j], pivot) > 0)
                j--;
            if (i <= j) {
                T temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
        }

        return i;
    }

    /**
     * sorts an array
     *
     * @param array array that should be sorted
     * @return true if sorting is successful and false otherwise
     */
    public abstract boolean sort(T[] array);
}