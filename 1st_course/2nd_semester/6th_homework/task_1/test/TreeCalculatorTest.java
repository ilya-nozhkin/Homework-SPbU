import org.junit.Test;
import s2_144.nozhkin.task6_1.Node;
import s2_144.nozhkin.task6_1.TreeBuilder;

import java.io.*;

import static org.junit.Assert.*;

public class TreeCalculatorTest {
    /** test set of expressions */
    private static final String[] EXPRESSIONS = {"10", "(+ 10 15)", "(+ (* 10 5) (/ (- 5 1) 2))"};

    /** right answers to EXPRESSIONS */
    private static final int[] VALUES = {10, 25, 52};

    /** test with illegal symbol */
    private static final String EXPRESSION_WITH_ILLEGAL_SYMBOL = "(+ g 10)";

    /** test with illegal structure */
    private static final String EXPRESSION_WITH_ILLEGAL_STRUCTURE = "(+ 10 (- 4 (* 4))";

    /**
     * checks that tree prints correctly
     *
     * @throws IOException shouldn't be thrown
     * @throws TreeBuilder.IllegalExpressionException shouldn't be thrown
     */
    @Test
    public void representationTest() throws IOException, TreeBuilder.IllegalExpressionException {
        for (String expression : EXPRESSIONS) {
            InputStream input = new ByteArrayInputStream(expression.getBytes("UTF-8"));
            OutputStream output = new ByteArrayOutputStream();

            Node root = TreeBuilder.buildTree(input);
            root.print(output);
            assertTrue(output.toString().equals(expression));
        }
    }

    /**
     * checks results of expressions evaluation
     *
     * @throws IOException shouldn't be thrown
     * @throws TreeBuilder.IllegalExpressionException shouldn't be thrown
     */
    @Test
    public void calculationTest() throws IOException, TreeBuilder.IllegalExpressionException {
        String[] expressions = {"10", "(+ 10 15)", "(+ (* 10 5) (/ (- 5 1) 2))"};

        for (int i = 0; i < EXPRESSIONS.length; i++) {
            InputStream input = new ByteArrayInputStream(EXPRESSIONS[i].getBytes("UTF-8"));

            Node root = TreeBuilder.buildTree(input);
            assertTrue(root.evaluate() == VALUES[i]);
        }
    }

    /**
     * checks behavior when source contains illegal symbol
     *
     * @throws IOException shouldn't be thrown
     * @throws TreeBuilder.IllegalExpressionException should be thrown because source contains illegal symbol
     */
    @Test(expected = TreeBuilder.IllegalExpressionException.class)
    public void illegalSymbolTest() throws IOException, TreeBuilder.IllegalExpressionException {
        InputStream input = new ByteArrayInputStream(EXPRESSION_WITH_ILLEGAL_SYMBOL.getBytes("UTF-8"));

        TreeBuilder.buildTree(input);
    }

    /**
     * checks behavior if source has illegal structure
     *
     * @throws IOException shouldn't be thrown
     * @throws TreeBuilder.IllegalExpressionException should be thrown because source has illegal structure
     */
    @Test(expected = TreeBuilder.IllegalExpressionException.class)
    public void illegalStructureTest() throws IOException, TreeBuilder.IllegalExpressionException {
        InputStream input = new ByteArrayInputStream(EXPRESSION_WITH_ILLEGAL_STRUCTURE.getBytes("UTF-8"));

        TreeBuilder.buildTree(input);
    }
}
