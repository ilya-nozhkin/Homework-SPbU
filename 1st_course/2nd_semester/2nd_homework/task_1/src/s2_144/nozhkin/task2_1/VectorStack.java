package s2_144.nozhkin.task2_1;

public class VectorStack<T> implements Stack<T> {
    private Vector<T> vector;

    public VectorStack() {
        vector = new Vector<T>();
    }

    @Override
    public void push(T value) {
        vector.add(value);
    }

    @Override
    public T pop() throws StackIsEmptyException {
        if (isEmpty())
            throw new StackIsEmptyException();

        int position = vector.size() - 1;
        T value = vector.get(position);
        vector.remove(position);

        return value;
    }

    @Override
    public boolean isEmpty() {
        return vector.size() == 0;
    }
}
