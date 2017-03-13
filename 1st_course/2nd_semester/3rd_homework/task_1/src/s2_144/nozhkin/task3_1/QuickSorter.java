package s2_144.nozhkin.task3_1;

import java.util.Comparator;

public class QuickSorter<T> extends ComparisonSorter<T> {
    public QuickSorter(Comparator<T> comparator) {
        super(comparator);
    }

    @Override
    public void sort(T[] array) {
        sort(array, 0, array.length - 1);
    }

    private void sort(T[] array, int first, int last) {
        T pivot = array[(first + last) / 2];
        int i = first;
        int j = last;

        while (i < j) {
            while (i < j && comparator.compare(array[i], pivot) < 0)
                i++;
            while (j > i && comparator.compare(array[j], pivot) > 0)
                j--;
            if (i < j) {
                T temp = array[i];
                array[i] = array[j];
                array[j] = temp;
            }
        }

        if (j - 1 - first > 0)
            sort(array, first, j - 1);
        if (last - i - 1 > 0)
            sort(array, i + 1, last);
    }
}
