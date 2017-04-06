package s2_144.nozhkin.task5_2;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ResourceBundle;

public class CalculatorController implements Initializable {
    /** array for matching operator and it's ID */
    private static final List<Character> OPERATION_SYMBOLS = Arrays.asList('+', '-', '*', '/');

    /** list of digit buttons */
    @FXML
    private ArrayList<Button> digitList;

    /** list of operation buttons */
    @FXML
    private ArrayList<Button> operationList;

    /** label that shows the state of calculator */
    @FXML
    private Label display;

    /** number that is typed before an operator / result of the previous operation */
    private String previousOperand = "0";

    /** number that is being typed now */
    private String currentOperand = "0";

    /** operation symbol */
    private char operation = '\0';

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        for (int i = 0; i < digitList.size(); i++) {
            final int digit = i;
            digitList.get(i).setOnAction(event -> typeDigit(digit));
        }

        for (int i = 0; i < operationList.size(); i++) {
            final int operationId = i;
            operationList.get(i).setOnAction(event -> typeOperation(OPERATION_SYMBOLS.get(operationId)));
        }

        printState();
    }

    /**
     * processes input from keyboard
     *
     * @param keyEvent information about typed key
     */
    public void onKeyTyped(KeyEvent keyEvent) {
        final char backspace = 8;
        final char enter = 13;

        char keyChar = keyEvent.getCharacter().charAt(0);

        if (keyChar >= '0' && keyChar <= '9') {
            typeDigit(keyChar - '0');
        } else if (OPERATION_SYMBOLS.contains(keyChar)) {
            typeOperation(keyChar);
        } else if (keyChar == '.' || keyChar == ',') {
            typeDot();
        } else if (keyChar == backspace) {
            eraseLast();
        } else if (keyChar == enter) {
            execute();
        }
    }

    /** reset the calculator */
    public void clean() {
        previousOperand = "0";
        currentOperand = "0";
        operation = '\0';

        printState();
    }

    /** erase last typed digit */
    public void eraseLast() {
        int length = currentOperand.length();
        if (length > 1) {
            currentOperand = currentOperand.substring(0, currentOperand.length() - 1);
        } else {
            currentOperand = "0";
        }

        printState();
    }

    /**
     * add digit to current operand
     *
     * @param digit digit that should be added
     */
    public void typeDigit(int digit) {
        if (currentOperand.equals("0"))
            currentOperand = "";
        currentOperand += Integer.toString(digit);

        printState();
    }

    /**
     * set the operator
     *
     * @param operation operator that should be set
     */
    public void typeOperation(char operation) {
        if (!currentOperand.equals("0")) {
            calculate(true);
        }
        this.operation = operation;
        printState();
    }

    /** type dot to the end of current operand */
    public void typeDot() {
        if (!currentOperand.contains(".")) {
            currentOperand = currentOperand + ".";
        }

        printState();
    }

    /** calculate the expression and put result to the current operand */
    public void execute() {
        calculate(false);
    }

    /**
     * calculates an expression that consists of two operands stored in
     * previousOperand and currentOperand and selected operator.
     *
     * @param toPrevious if this parameter is true result should be stored to previousOperand else to currentOperand
     */
    public void calculate(boolean toPrevious) {
        String resultString = currentOperand;
        if (operation != '\0') {
            double left = Double.valueOf(previousOperand);
            double right = Double.valueOf(currentOperand);

            double result = Calculator.calculate(operation, left, right);
            resultString = result == 0 ? "0" : Double.toString(result);
        }

        if (toPrevious) {
            previousOperand = resultString;
            currentOperand = "0";
        } else {
            previousOperand = "0";
            currentOperand = resultString;
        }

        operation = '\0';

        printState();
    }

    /** forms current state of calculator and prints it on the display */
    public void printState() {
        String state = "";

        if (!previousOperand.equals("0")) {
            state += previousOperand + '\n';
        }

        if (operation != '\0') {
            state += Character.toString(operation) + '\n';
        }

        state += currentOperand;

        display.setText(state);
    }
}
