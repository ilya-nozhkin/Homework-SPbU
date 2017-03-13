package s2_144.nozhkin.task2_1;

public class StackCalculator {
    public static double calculate(String expression) throws UnknownOperatorException, NotEnoughArgumentsException {
        VectorStack<Double> stack = new VectorStack<Double>();

        String[] tokens = expression.split(" +");

        for (int i = 0; i < tokens.length; i++) {
            if (isOperation(tokens[i])) {
                char operator = tokens[i].charAt(0);

                double right = 0;
                double left = 0;
                try {
                    right = stack.pop();
                    left = stack.pop();
                } catch (Stack.StackIsEmptyException e) {
                    throw new NotEnoughArgumentsException(operator, i);
                }

                stack.push(evaluate(operator, left, right));
            } else {
                stack.push(Double.valueOf(tokens[i]));
            }
        }

        double result = 0;
        try {
            result = stack.pop();
        } catch (Stack.StackIsEmptyException e) {
            e.printStackTrace();
        }
        return result;
    }

    private static double evaluate(char operator, double left, double right) throws UnknownOperatorException {
        double result = 0;
        switch (operator) {
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
            default:
                throw new UnknownOperatorException(operator);
        }
        return result;
    }

    private static boolean isOperation(String token) {
        if (token.length() != 1)
            return false;

        char symbol = token.charAt(0);

        return symbol < '0' || symbol > '9';
    }

    public static class UnknownOperatorException extends Exception {
        public UnknownOperatorException(char operator) {
            super("Unknown operator: '" + operator + "'");
        }
    }

    public static class NotEnoughArgumentsException extends Exception {
        public NotEnoughArgumentsException(char operator, int token) {
            super("Not enough arguments for operator '" + operator + "' at token " + token);
        }
    }
}
