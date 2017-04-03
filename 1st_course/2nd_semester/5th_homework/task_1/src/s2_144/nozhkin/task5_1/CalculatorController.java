package s2_144.nozhkin.task5_1;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.util.StringConverter;

import java.net.URL;
import java.util.ResourceBundle;

/** javafx controller which provides calculator's functions to GUI */
public class CalculatorController implements Initializable {
    /** text field which contains the first operand */
    @FXML
    private Spinner<Double> leftOperand;

    /** text field which contains the second operand */
    @FXML
    private Spinner<Double> rightOperand;

    /** combo box which is used for choosing the operation */
    @FXML
    private ComboBox<String> operation;

    /** label that stores the result */
    @FXML
    private Label result;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        operation.getItems().addAll("+", "-", "*", "/");
        operation.getSelectionModel().select(0);

        leftOperand.setValueFactory(new SpinnerValueFactory.DoubleSpinnerValueFactory(
                Double.MIN_VALUE, Double.MAX_VALUE, 0));

        rightOperand.setValueFactory(new SpinnerValueFactory.DoubleSpinnerValueFactory(
                Double.MIN_VALUE, Double.MAX_VALUE, 0));

        ChangeListener<Double> listener = (observable, oldValue, newValue) -> calculate();

        leftOperand.valueProperty().addListener(listener);
        rightOperand.valueProperty().addListener(listener);

        calculate();
    }

    /**
     * Calculates result using values from operand's text areas and operation combo box.
     * Called when operands or operation have been changed
     */
    public void calculate() {
        try {
            double leftValue = leftOperand.getValue();
            double rightValue = rightOperand.getValue();
            char operationId = operation.getValue().charAt(0);

            double resultValue = Calculator.calculate(operationId, leftValue, rightValue);
            result.setText(Double.toString(resultValue));
        } catch (NumberFormatException e) {}
    }
}
