package s2_144.nozhkin.task3_1;

import java.util.Comparator;

public class BubbleSorter<T> extends ComparisonSorter<T> {
    public BubbleSorter(Comparator<T> comparator) {
        super(comparator);
    }

    @Override
    public void sort(T[] array) {
        for (int i = 0; i < array.length - 1; i++) {
            for (int j = 0; j < array.length - 1 - i; j++) {
                if (comparator.compare(array[j], array[j + 1]) > 0) {
                    T temp = array[j];
                    array[j] = array[j + 1];
                    array[j + 1] = temp;
                }
            }
        }
    }
}
