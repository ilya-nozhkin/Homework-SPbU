import javafx.application.Application;
import javafx.event.EventType;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Spinner;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.PickResult;
import javafx.stage.Stage;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import s2_144.nozhkin.task5_1.Main;


public class CalculatorTest extends Application {
    private static Stage primaryStage;
    private static AssertionError assertion = null;

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

    /** performs some simple actions with controls and checks results */
    public void calculationTest() {
        Spinner<Double> leftOperand = findNode("#leftOperand");
        Spinner<Double> rightOperand = findNode("#rightOperand");
        ComboBox<String> operation = findNode("#operation");
        Label result = findNode("#result");

        leftOperand.getValueFactory().setValue(10.0);
        rightOperand.getValueFactory().setValue(2.0);

        operation.getSelectionModel().select("+");
        Assert.assertTrue(result.getText().equals("12.0"));

        operation.getSelectionModel().select("-");
        Assert.assertTrue(result.getText().equals("8.0"));

        operation.getSelectionModel().select("*");
        Assert.assertTrue(result.getText().equals("20.0"));

        operation.getSelectionModel().select("/");
        Assert.assertTrue(result.getText().equals("5.0"));
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        this.primaryStage = primaryStage;

        Parent root = FXMLLoader.load(Main.class.getResource("Calculator.fxml"));
        primaryStage.setTitle("Simple calculator");
        primaryStage.setScene(new Scene(root));
        primaryStage.setResizable(false);
        primaryStage.show();

        try {
            calculationTest();
        } catch (AssertionError e) {
            assertion = e;
            primaryStage.close();
        }

        primaryStage.close();
    }
}
