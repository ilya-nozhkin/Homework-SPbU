package s2_144.nozhkin.task5_2;

/** class which can do some arithmetic operations */
public class Calculator {
    /**
     * calculates a result of an arithmetic operation.
     *
     * @param operation '+', '-', '*', '/'
     * @param left operand which should be located on the left of an operator
     * @param right operand which should be located on the left of an operator
     * @return result of an arithmetic operation
     */
    public static double calculate(char operation, double left, double right) {
        double result = 0;
        switch (operation) {
            case '+':
                result = left + right;
                break;
            case '-':
                result = left - right;
                break;
            case '*':
                result = left * right;
                break;
            case '/':
                result = left / right;
                break;
        }
        return result;
    }
}