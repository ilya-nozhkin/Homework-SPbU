package s2_144.nozhkin.task3_2;

import java.io.*;

public class FileSpiralPrinter extends SpiralConverter {
    FileWriter writer = null;
    File file = null;

    public FileSpiralPrinter(File file) {
        this.file = file;
    }

    @Override
    protected void writingStarted() throws IOException {
        writer = new FileWriter(file);
    }

    @Override
    protected void writingFinished() throws IOException {
        writer.close();
    }

    @Override
    protected void write(int data) throws IOException {
        writer.write(Integer.toString(data) + ' ');
    }
}
