package s2_144.nozhkin.task2_1;

public class StackCalculator {
    public static double calculate(String expression) {
        VectorStack<Double> stack = new VectorStack<Double>();

        String[] tokens = expression.split(" +");

        for (int i = 0; i < tokens.length; i++) {
            if (isOperation(tokens[i])) {
                char operator = tokens[i].charAt(0);

                if (stack.isEmpty())
                    throw new NotEnoughArgumentsException(operator, i);

                double right = stack.pop();

                if (stack.isEmpty())
                    throw new NotEnoughArgumentsException(operator, i);

                double left = stack.pop();

                stack.push(evaluate(operator, left, right));
            } else {
                stack.push(Double.valueOf(tokens[i]));
            }
        }

        return stack.pop();
    }

    private static double evaluate(char operator, double left, double right) {
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

    public static class UnknownOperatorException extends RuntimeException {
        public UnknownOperatorException(char operator) {
            super("Unknown operator: '" + operator + "'");
        }
    }

    public static class NotEnoughArgumentsException extends RuntimeException {
        public NotEnoughArgumentsException(char operator, int token) {
            super("Not enough arguments for operator '" + operator + "' at token " + token);
        }
    }
}
