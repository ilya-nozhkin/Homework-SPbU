package s2_144.nozhkin.task3_1;

import java.util.Comparator;

public class SelectionSorter<T> extends ComparisonSorter<T> {
    public SelectionSorter(Comparator<T> comparator) {
        super(comparator);
    }

    @Override
    public void sort(T[] array) {
        for (int i = 0; i < array.length - 1; i++) {
            int min = i;
            for (int j = i + 1; j < array.length; j++) {
                if (comparator.compare(array[j], array[min]) < 0) {
                    min = j;
                }
            }
            if (min > i) {
                T temp = array[min];
                array[min] = array[i];
                array[i] = temp;
            }
        }
    }
}
