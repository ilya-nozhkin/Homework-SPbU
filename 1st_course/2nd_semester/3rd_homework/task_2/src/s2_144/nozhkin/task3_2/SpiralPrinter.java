package s2_144.nozhkin.task3_2;

public interface SpiralPrinter {
    void print(int[][] array) throws Exception;

    class ArrayIsNotPrintableException extends Exception {
        public ArrayIsNotPrintableException(String why) {
            super("Array is not printable because " + why);
        }
    }
}
