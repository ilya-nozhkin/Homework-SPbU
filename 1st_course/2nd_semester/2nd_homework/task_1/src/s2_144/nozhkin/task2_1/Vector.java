package s2_144.nozhkin.task2_1;

public class Vector<T> {
    private static final int INITIAL_LENGTH = 4;
    private static final float INFLATE_FACTOR = 1.618f;

    private T[] buffer = null;
    private int count = 0;

    public Vector() {
        buffer = (T[]) new Object[INITIAL_LENGTH];
        count = 0;
    }

    public int size() {
        return count;
    }

    public void add(T value) {
        if (count == buffer.length)
            inflate();

        buffer[count] = value;
        count++;
    }

    public void remove(int position) {
        if (position < 0 && position >= count)
            throw new IndexOutOfBoundsException();

        for (int i = position; i < count - 1; i++)
            buffer[i] = buffer[i + 1];

        count--;
    }

    public T get(int position) {
        if (position < 0 && position >= count)
            throw new IndexOutOfBoundsException();

        return buffer[position];
    }

    private void inflate() {
        int newLength = (int) (INFLATE_FACTOR * buffer.length);
        T[] newBuffer = (T[]) new Object[newLength];

        for (int i = 0; i < count; i++)
            newBuffer[i] = buffer[i];

        buffer = newBuffer;
    }
}
