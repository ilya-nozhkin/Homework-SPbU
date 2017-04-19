package s2_144.nozhkin.task6_1;

import java.io.IOException;
import java.io.OutputStream;

/** Node that represents a number */
public class Number implements Node {

    /** constructor for fast initialization, also it forbids to use a default constructor
     * (it is needed because there shouldn't be uninitialized numbers) */
    public Number(int value) {
        this.value = value;
    }

    @Override
    public void print(OutputStream stream) throws IOException {
        stream.write(Integer.toString(value).getBytes());
    }

    @Override
    public int evaluate() {
        return value;
    }

    /** value of the number */
    private int value;
}
