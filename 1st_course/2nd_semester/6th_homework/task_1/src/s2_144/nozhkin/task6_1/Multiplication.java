package s2_144.nozhkin.task6_1;

public class Multiplication extends Operator {
    @Override
    public int evaluate() {
        return leftOperand.evaluate() * rightOperand.evaluate();
    }

    @Override
    protected char getOperatorSymbol() {
        return '*';
    }
}
