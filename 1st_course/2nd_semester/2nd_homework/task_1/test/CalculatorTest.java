import org.junit.Assert;
import org.junit.Test;
import s2_144.nozhkin.task2_1.StackCalculator;

public class CalculatorTest {
    @Test
    public void usualExpressionTest() throws StackCalculator.UnknownOperatorException,
                                             StackCalculator.NotEnoughArgumentsException {
        Assert.assertTrue(StackCalculator.calculate("12 3 +") == 15);
        Assert.assertTrue(StackCalculator.calculate("12 3 -") == 9);
        Assert.assertTrue(StackCalculator.calculate("12 3 *") == 36);
        Assert.assertTrue(StackCalculator.calculate("12 3 /") == 4);

        Assert.assertTrue(StackCalculator.calculate("-12 3 +") == -9);
        Assert.assertTrue(StackCalculator.calculate("-12 3 -") == -15);
        Assert.assertTrue(StackCalculator.calculate("-12 3 *") == -36);
        Assert.assertTrue(StackCalculator.calculate("-12 3 /") == -4);

        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 +") == -1);
        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 -") == -2);
        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 *") == -0.75);
        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 /") == -3);

        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 / 2 3 * +") == 3);
        Assert.assertTrue(StackCalculator.calculate("-1.5 0.5 - 0.25 / 5 6 + *") == -88);
    }

    @Test(expected = StackCalculator.NotEnoughArgumentsException.class)
    public void illegalExpressionTest() throws StackCalculator.UnknownOperatorException,
                                               StackCalculator.NotEnoughArgumentsException {
        StackCalculator.calculate("2 4 + /");
    }

    @Test(expected = StackCalculator.UnknownOperatorException.class)
    public void unknownOperatorTest() throws StackCalculator.UnknownOperatorException,
                                             StackCalculator.NotEnoughArgumentsException {
        StackCalculator.calculate("2 4 + 2 ^");
    }
}
