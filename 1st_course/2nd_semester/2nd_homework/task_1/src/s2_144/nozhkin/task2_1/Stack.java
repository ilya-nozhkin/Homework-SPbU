package s2_144.nozhkin.task2_1;

public interface Stack<T> {
    void push(T value);
    T pop() throws StackIsEmptyException;
    boolean isEmpty();

    class StackIsEmptyException extends Exception {
        public StackIsEmptyException() {
            super("'pop' has been called but stack is empty");
        }
    }
}
