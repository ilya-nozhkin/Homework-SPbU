package s2_144.nozhkin.task2_1;

public interface Stack<T> {
    void    push(T value);
    T       pop();
    boolean isEmpty();

    class StackIsEmptyException extends RuntimeException {
        public StackIsEmptyException() {
            super("'pop' has been called but stack is empty");
        }
    }
}
