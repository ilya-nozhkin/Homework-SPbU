package s2_144.nozhkin.task3_2;

public abstract class SpiralConverter implements SpiralPrinter {
    @Override
    public void print(int[][] array) throws Exception {
        int height = array.length;
        int width = array[0].length;

        int y = height / 2;
        int x = width / 2;

        int dy = 0;
        int dx = -1;

        if (width == 0 || height == 0)
            throw new ArrayIsNotPrintableException("it has not columns or rows");

        if (height == 1 && x != 0) {
            throw new ArrayIsNotPrintableException("it has only one row and its width is greater than 2");
        }

        if (width == 1) {
            dy = -1;
            dx = 0;

            if (y != 0)
                throw new ArrayIsNotPrintableException("it has only one column and its height is greater than 2");
        }

        writingStarted();

        int nextLength = 1;
        boolean extentionNeeded = false;
        while (y >= 0 && y < height && x >= 0 && x < width) {
            for (int i = 0; i < nextLength; i++) {
                write(array[y][x]);
                x += dx;
                y += dy;
            }

            int temp = dx;
            dx = -dy;
            dy = temp;

            if (extentionNeeded) {
                nextLength++;
            }

            extentionNeeded = !extentionNeeded;
        }

        writingFinished();
    }

    protected void writingStarted() throws Exception {}
    protected void writingFinished() throws Exception {}
    protected abstract void write(int data) throws Exception;
}
