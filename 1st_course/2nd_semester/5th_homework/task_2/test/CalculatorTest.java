import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.stage.Stage;
import org.junit.Assert;
import org.junit.Test;
import s2_144.nozhkin.task5_2.Main;

import java.util.ArrayList;

/** some simple tests for GUI */
public class CalculatorTest extends Application {
    /** JavaFX stage */
    private static Stage primaryStage;

    /** field that is used for sending exception thrown in JavaFX start to the test method */
    private static AssertionError assertion = null;

    /** Label that shows the state of calculator */
    Label display;

    /** Button that cleans calculator */
    Button clean;

    /** Button that erases last symbol */
    Button backspace;

    /** Button that types the dot */
    Button dot;

    /** Button that calculates current expression */
    Button calculate;

    /** list of digit buttons */
    ArrayList<Button> digits;

    /** list of buttons with operators */
    ArrayList<Button> operations;

    /** starts the JavaFX application and waits for assertion fails */
    @Test
    public void test() {
        assertion = null;
        CalculatorTest.launch(CalculatorTest.class);
        if (assertion != null)
            throw assertion;
    }

    /** finds node on scene
     *
     * @param selector fx:id of the node
     * @param <T> type of the node
     * @return node
     */
    public static <T> T findNode(String selector) {
        return (T) primaryStage.getScene().lookup(selector);
    }

    /** input two numbers and plus, check display */
    public void plusTest() {
        digits.get(5).fire();
        digits.get(4).fire();

        Assert.assertTrue(display.getText().equals("54"));

        operations.get(0).fire();

        Assert.assertTrue(display.getText().equals("54\n+\n0"));

        digits.get(1).fire();
        digits.get(6).fire();

        Assert.assertTrue(display.getText().equals("54\n+\n16"));
    }

    /** calculates previous expression and enters a new one with minus */
    public void minusTest() {
        operations.get(1).fire();

        Assert.assertTrue(display.getText().equals("70.0\n-\n0"));

        digits.get(1).fire();
        digits.get(0).fire();

        Assert.assertTrue(display.getText().equals("70.0\n-\n10"));
    }

    /** calculates previous expression and enters a new one with multiplication, also tries to use backspace */
    public void multiplicationTest() {
        operations.get(2).fire();

        Assert.assertTrue(display.getText().equals("60.0\n*\n0"));

        dot.fire();
        digits.get(5).fire();
        backspace.fire();
        digits.get(1).fire();

        Assert.assertTrue(display.getText().equals("60.0\n*\n0.1"));
    }

    /** calculates previous expression, enters a new one with division, and calculates it */
    public void divisionTest() {
        operations.get(3).fire();

        Assert.assertTrue(display.getText().equals("6.0\n/\n0"));

        digits.get(8).fire();

        Assert.assertTrue(display.getText().equals("6.0\n/\n8"));

        calculate.fire();

        Assert.assertTrue(display.getText().equals("0.75"));
    }

    /** performs some simple actions with controls and checks results */
    public void buttonTest() {
        display = findNode("#display");
        clean = findNode("#clean");
        backspace = findNode("#backspace");
        dot = findNode("#dot");
        calculate = findNode("#calculate");

        digits = new ArrayList<>();
        operations = new ArrayList<>();

        for (int i = 0; i < 10; i++) {
            digits.add(findNode("#digit" + Integer.toString(i)));
        }

        operations.add(0, findNode("#plus"));
        operations.add(1, findNode("#minus"));
        operations.add(2, findNode("#multiplication"));
        operations.add(3, findNode("#division"));

        plusTest();
        minusTest();
        multiplicationTest();
        divisionTest();

        clean.fire();

        Assert.assertTrue(display.getText().equals("0"));
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        this.primaryStage = primaryStage;

        Parent root = FXMLLoader.load(Main.class.getResource("CalculatorLayout.fxml"));
        primaryStage.setTitle("Calculator");
        primaryStage.setScene(new Scene(root));
        primaryStage.setResizable(false);
        primaryStage.show();

        try {
            buttonTest();
        } catch (AssertionError e) {
            assertion = e;
            primaryStage.close();
        }

        primaryStage.close();
    }
}
