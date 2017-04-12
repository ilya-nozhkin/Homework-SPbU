package s2_144.nozhkin.task6_1;

import java.io.IOException;
import java.io.OutputStream;

/** base interface for all nodes */
public interface Node {
    /**
     * prints node and all it's children (if there are)
     *
     * @param stream stream where information will be printed
     * @throws IOException if stream is not writable
     */
    void print(OutputStream stream) throws IOException;

    /**
     * calculates a result of the expression tree where this node is root
     *
     * @return result of the calculation
     */
    int evaluate();
}
