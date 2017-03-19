package s2_144.nozhkin.task4_2;

public interface Map<K, V> {
    void set(K key, V value);
    void remove(K key) throws NoSuchElementException;
    V get(K key);
    int size();

    Statistics getStatistics();

    class Statistics {
        public int cells;
        public double loadFactor;
        public int collisions;
        public int maxListLength;
    }

    class NoSuchElementException extends Exception {
        public NoSuchElementException(String key) {
            super("Map doesn't contain key " + key);
        }
    }
}
