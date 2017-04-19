package s2_144.nozhkin.task6_1;

import java.io.IOException;
import java.io.InputStream;

/** class that builds a tree by expression given from string in such format:
 * (operator tree/number tree/number)
 * where tree is (operator tree/number tree/number)
 */
public class TreeBuilder {
    /** checks if this symbol is digit
     *
     * @param symbol character
     * @return true if symbol is digit or false if not
     */
    private static boolean isDigit(char symbol) {
        return symbol >= '0' && symbol <= '9';
    }

    /**
     * reads a number from given stream (Be careful! It also reads one symbol after a number)
     *
     * @param stream where it reads from
     * @param firstSymbol symbol that has been read before caller understood that current node is number
     * @return number node if reading is successful and null if not
     * @throws IOException if stream is not readable
     */
    private static Number readNumber(InputStream stream, char firstSymbol) throws IOException {
        if (!isDigit(firstSymbol)) {
            return null;
        }

        int value = firstSymbol - '0';
        while (stream.available() > 0) {
            char symbol = (char) stream.read();

            if (!isDigit(symbol)) {
                break;
            }

            value *= 10;
            value += (symbol - '0');
        }
        return new Number(value);
    }

    /**
     * reads an operator from given stream (Be careful! It also reads one symbol after closing bracket)
     *
     * @param stream where it reads from
     * @return operator node if reading is successful and null if not
     * @throws IOException if stream is not readable
     * @throws IllegalExpressionException if expression contains a mistake
     */
    private static Operator readOperator(InputStream stream) throws IOException, IllegalExpressionException {
        char symbol = (char) stream.read();

        Operator operator = null;
        switch(symbol) {
            case '+':
                operator = new Plus();
                break;
            case '-':
                operator = new Minus();
                break;
            case '*':
                operator = new Multiplication();
                break;
            case '/':
                operator = new Division();
                break;
        }

        if (operator == null) {
            return null;
        }

        stream.skip(1);
        operator.setLeftOperand(readNode(stream));
        operator.setRightOperand(readNode(stream));
        stream.skip(1);

        return operator;
    }

    /**
     * reads a node from given stream (Be careful! It also reads one symbol after it)
     *
     * @param stream where it reads from
     * @return node object if reading is successful and null if not
     * @throws IOException if stream is not readable
     * @throws IllegalExpressionException if expression contains a mistake
     */
    private static Node readNode(InputStream stream) throws IOException, IllegalExpressionException {
        char symbol = (char) stream.read();

        Node node = null;

        if (symbol >= '0' && symbol <= '9') {
            node = readNumber(stream, symbol);
        } else if (symbol == '(') {
            node = readOperator(stream);
        }

        if (node != null) {
            return node;
        }

        throw new IllegalExpressionException();
    }

    /**
     * builds a tree by expression read from given stream
     *
     * @param stream where it reads from
     * @return root object of the tree if reading is successful and null if not
     * @throws IOException if stream is not readable
     * @throws IllegalExpressionException if expression contains a mistake
     */
    public static Node buildTree(InputStream stream) throws IOException, IllegalExpressionException {
        return readNode(stream);
    }

    /** is thrown when expression contains a mistake */
    public static class IllegalExpressionException extends Exception {
        public IllegalExpressionException() {
            super("Cannot build tree because expression has illegal structure");
        }
    }
}
