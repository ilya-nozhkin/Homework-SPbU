package s2_144.nozhkin.task6_1;

import java.io.IOException;
import java.io.OutputStream;

/** abstract class that represents an operator node (entity that has operands and can operate with them) */
public abstract class Operator implements Node {
    /** left operand in infix form, or first in prefix one */
    protected Node leftOperand;

    /** right operand in infix form, or second in prefix one */
    protected Node rightOperand;

    void setLeftOperand(Node node) {
        leftOperand = node;
    }

    void setRightOperand(Node node) {
        rightOperand = node;
    }

    @Override
    public void print(OutputStream stream) throws IOException {
        stream.write('(');
        stream.write(getOperatorSymbol());
        stream.write(' ');

        leftOperand.print(stream);
        stream.write(' ');
        rightOperand.print(stream);

        stream.write(')');
    }

    /**
     * @return a character that represents this operator
     */
    protected abstract char getOperatorSymbol();
}
