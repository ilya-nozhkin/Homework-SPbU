import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.stage.Stage;
import org.junit.Test;
import s2_144.nozhkin.task7_2.Main;

import static org.junit.Assert.*;

import java.util.ArrayList;

/** some simple tests for GUI */
public class TicTacToeTest extends Application {
    /** JavaFX stage */
    private static Stage primaryStage;

    /** field that is used for sending exception thrown in JavaFX start to the test method */
    private static AssertionError assertion = null;

    /** Restart button */
    Button restart;

    /** list of cell buttons */
    ArrayList<Button> cells;

    /** starts the JavaFX application and waits for assertion fails */
    @Test
    public void test() {
        assertion = null;
        TicTacToeTest.launch(TicTacToeTest.class);
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

    /** checks that marks alternate */
    public void markOrderTest() {
        boolean turn = true;
        for (int i = 0; i < 9; i++) {
            cells.get(i).fire();
            assertTrue(cells.get(i).getText().equals(turn ? "X" : "O"));
            turn = !turn;
        }
    }

    /** checks that restart button cleans all cells*/
    public void restartTest() {
        restart.fire();
        for (int i = 0; i < 9; i++) {
            assertTrue(cells.get(i).getText().length() == 0);
        }
    }

    /** checks that you can't switch mark on cell */
    public void doubleMarkTest() {
        restart.fire();

        cells.get(0).fire();
        assertTrue(cells.get(0).getText().equals("X"));
        cells.get(0).fire();
        assertTrue(cells.get(0).getText().equals("X"));

        cells.get(1).fire();
        assertTrue(cells.get(1).getText().equals("O"));
        cells.get(1).fire();
        assertTrue(cells.get(1).getText().equals("O"));
    }

    /** performs some simple actions with controls and checks results */
    public void buttonTest() {
        restart = findNode("#restartButton");
        cells = new ArrayList<>();

        for (int i = 0; i < 9; i++) {
            cells.add(findNode("#cell" + Integer.toString(i)));
        }

        markOrderTest();
        restartTest();
        doubleMarkTest();
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        this.primaryStage = primaryStage;

        Parent root = FXMLLoader.load(Main.class.getClassLoader().getResource("layout.fxml"));
        primaryStage.setTitle("XO");
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
