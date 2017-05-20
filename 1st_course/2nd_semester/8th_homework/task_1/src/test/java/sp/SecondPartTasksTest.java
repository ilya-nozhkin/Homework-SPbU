package sp;

import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

import static org.junit.Assert.*;

public class SecondPartTasksTest {
    private Map<String, List<String>> compositions = new HashMap<>();
    private List<Map<String, Integer>> orders = new ArrayList<>();

    @Before
    public void initialize() {
        compositions.put("Vergilius", Arrays.asList(
                "Furens quid femina possit",
                "Fuimus Troes, fuit Ilium",
                "Felix qui potuit rerum cognoscere causas"));

        compositions.put("Seneca", Arrays.asList(
                "Aditum nocendi perfido praestat fides",
                "Calamitas virtutis occasio",
                "Deest remedii locus, ubi, quae vitia fuerunt, mores fiunt"));

        compositions.put("Horatius", Arrays.asList(
                "Brevis esse laboro, obscurus fiо",
                "In vitium ducit culpae fuga",
                "Damnosa quid non inminuit dies?"));

        Map<String, Integer> firstOrder = new HashMap<>();
        firstOrder.put("D. Kahneman, Thinking, Fast and Slow", 20);
        firstOrder.put("S. Freud, The Interpretation of Dreams", 10);
        firstOrder.put("C. Jung, Man and His Symbols", 15);

        Map<String, Integer> secondOrder = new HashMap<>();
        secondOrder.put("S. Freud, The Interpretation of Dreams", 10);
        secondOrder.put("D. Myers, Psychology", 20);
        secondOrder.put("C. Rogers, On Becoming a Person", 15);

        Map<String, Integer> thirdOrder = new HashMap<>();
        thirdOrder.put("C. Jung, Man and His Symbols", 15);
        thirdOrder.put("D. Myers, Psychology", 10);
        thirdOrder.put("A. Freud, The Ego and the Mechanisms of Defence", 10);

        orders.addAll(Arrays.asList(firstOrder, secondOrder, thirdOrder));
    }

    @Test
    public void testFindQuotes() {
        for (Map.Entry<String, List<String>> entry : compositions.entrySet()) {
            try {
                FileWriter writer = new FileWriter(new File(entry.getKey() + ".txt"));
                for (String string : entry.getValue()) {
                    writer.write(string + '\n');
                }
                writer.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        assertEquals(Arrays.asList(
                "Fuimus Troes, fuit Ilium",
                "Felix qui potuit rerum cognoscere causas",
                "Damnosa quid non inminuit dies?"
                ), SecondPartTasks.findQuotes(
                        Arrays.asList("Vergilius.txt", "Seneca.txt", "Horatius.txt"), "uit"));

        assertEquals(Arrays.asList(
                "Fuimus Troes, fuit Ilium",
                "Felix qui potuit rerum cognoscere causas",
                "Aditum nocendi perfido praestat fides",
                "In vitium ducit culpae fuga"
                ), SecondPartTasks.findQuotes(
                        Arrays.asList("Vergilius.txt", "Seneca.txt", "Horatius.txt"), "um"));

        assertEquals(Arrays.asList(),
                SecondPartTasks.findQuotes(Arrays.asList("Seneca.txt", "Horatius.txt"), "femina"));

        assertEquals(Arrays.asList(),
                SecondPartTasks.findQuotes(Arrays.asList(), "obscurus"));
    }

    @Test
    public void testPiDividedBy4() {
        assertTrue(SecondPartTasks.piDividedBy4() * 4 - 3.14 < 0.005);
    }

    @Test
    public void testFindPrinter() {
        assertEquals("Seneca", SecondPartTasks.findPrinter(compositions));
        assertEquals(null, SecondPartTasks.findPrinter(new HashMap<>()));
    }

    @Test
    public void testCalculateGlobalOrder() {
        Map<String, Integer> expected = new HashMap<>();
        expected.put("D. Kahneman, Thinking, Fast and Slow", 20);
        expected.put("S. Freud, The Interpretation of Dreams", 20);
        expected.put("C. Jung, Man and His Symbols", 30);
        expected.put("D. Myers, Psychology", 30);
        expected.put("C. Rogers, On Becoming a Person", 15);
        expected.put("A. Freud, The Ego and the Mechanisms of Defence", 10);

        assertEquals(expected, SecondPartTasks.calculateGlobalOrder(orders));
    }
}