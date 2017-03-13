import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import s2_144.nozhkin.task3_2.ConsoleSpiralPrinter;
import s2_144.nozhkin.task3_2.FileSpiralPrinter;
import s2_144.nozhkin.task3_2.SpiralPrinter;

import java.io.*;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class SpiralPrinterTest {
    private static final int[][] TEST_SET = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    private static final String EXPECTED = "5 4 1 2 3 6 9 8 7 ";

    private String getConsoleResult() throws Exception {
        ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        PrintStream printStream = new PrintStream(byteStream);

        PrintStream backup = System.out;
        System.setOut(printStream);

        SpiralPrinter printer = new ConsoleSpiralPrinter();
        printer.print(TEST_SET);

        System.setOut(backup);
        printStream.close();

        String answer = byteStream.toString();
        return answer;
    }

    private String getFileResult() throws Exception {
        File file = new File("test.txt");
        SpiralPrinter printer = new FileSpiralPrinter(file);
        printer.print(TEST_SET);

        FileReader fileReader = new FileReader(file);
        BufferedReader reader = new BufferedReader(fileReader);

        String answer = reader.readLine();

        reader.close();
        file.delete();

        return answer;
    }

    @Test
    public void functionalTest() throws Exception {
        String answer = getConsoleResult();
        Assert.assertTrue(answer.equals(EXPECTED));
    }

    @Test
    public void sourceMatchingTest() throws Exception {
        String consoleResult = getConsoleResult();
        String fileResult = getFileResult();
        Assert.assertTrue(consoleResult.equals(fileResult));
    }
}
