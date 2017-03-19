package s2_144.nozhkin.task4_2;

import java.util.ListIterator;

public class HashTable<K, V> implements Map<K, V> {
    public static final int INITIAL_SIZE = 7;
    public static final float INFLATE_FACTOR = 1.618f;
    public static final float CRITICAL_LOAD_FACTOR = 0.5f;

    private UniqueList<Pair>[] cells = null;
    private int cellsNumber = 0;

    private int pairs = 0;
    private int collisions = 0;
    private int maxListLength = 0;

    private Hasher<K> hasher = null;

    public HashTable(Hasher hasher) {
        cells = allocateCells(INITIAL_SIZE);
        cellsNumber = INITIAL_SIZE;
        this.hasher = hasher;
    }

    public void setHasher(Hasher hasher) {
        UniqueList<Pair>[] oldCells = cells;
        this.hasher = hasher;

        refill(oldCells);
    }

    @Override
    public void set(K key, V value) {
        putWithoutInflation(key, value);

        if (getLoadFactor() > CRITICAL_LOAD_FACTOR) {
            inflate();
        }
    }

    @Override
    public V get(K key) {
        int position = hasher.hash(key) % cellsNumber;

        if (cells[position] == null) {
            return null;
        }

        for (Pair pair : cells[position]) {
            if (pair.key.equals(key)) {
                return pair.value;
            }
        }

        return null;
    }

    @Override
    public void remove(K key) throws NoSuchElementException {
        int position = hasher.hash(key) % cellsNumber;

        if (cells[position] == null) {
            return;
        }

        ListIterator<Pair> iterator = cells[position].iterator();
        while (iterator.hasNext()) {
            Pair pair = iterator.next();
            if (pair.key.equals(key)) {
                iterator.remove();
                return;
            }
        }

        throw new NoSuchElementException(key.toString());
    }

    @Override
    public int size() {
        return pairs;
    }

    @Override
    public Statistics getStatistics() {
        Statistics statistics = new Statistics();
        statistics.cells = cellsNumber;
        statistics.collisions = collisions;
        statistics.loadFactor = getLoadFactor();
        statistics.maxListLength = maxListLength;
        return statistics;
    }

    private void putWithoutInflation(K key, V value) {
        int position = hasher.hash(key) % cellsNumber;

        if (cells[position] == null) {
            cells[position] = new UniqueList<>();
        }

        UniqueList<Pair> cell = cells[position];

        try {
            cell.add(new Pair(key, value));

            pairs++;
            if (cell.size() > 1)
                collisions++;
        } catch (UniqueList.AlreadyExistsException e) {
            for (Pair pair : cell) {
                if (pair.key.equals(key)) {
                    pair.value = value;
                }
            }
        }

        if (cell.size() > maxListLength) {
            maxListLength = cell.size();
        }
    }

    private UniqueList<Pair>[] allocateCells(int number) {
        return (UniqueList<Pair>[]) new UniqueList[number];
    }

    private float getLoadFactor() {
        return (float) pairs / cellsNumber;
    }

    private void refill(UniqueList<Pair>[] data) {
        cells = allocateCells(cellsNumber);

        pairs = 0;
        collisions = 0;
        maxListLength = 0;

        for (UniqueList<Pair> cell : data) {
            if (cell != null) {
                for (Pair pair : cell) {
                    putWithoutInflation(pair.key, pair.value);
                }
            }
        }
    }

    private void inflate() {
        UniqueList<Pair>[] oldCells = cells;
        cellsNumber = (int) (cellsNumber * INFLATE_FACTOR);

        refill(oldCells);
    }

    private class Pair {
        private K key;
        private V value;

        public Pair(K key, V value) {
            this.key = key;
            this.value = value;
        }
    }
}
