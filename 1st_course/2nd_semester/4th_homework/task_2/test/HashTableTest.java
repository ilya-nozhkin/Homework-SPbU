import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import s2_144.nozhkin.task4_2.*;

import java.util.Random;

public class HashTableTest {
    private static final int TEST_SET_SIZE = 1024;
    private static final int INDEX_SCALE = 1618;
    private static final int ERASE_START = 200;
    private static final int ERASE_END = 400;

    private String[] keys = new String[TEST_SET_SIZE];
    private int[] values = new int[TEST_SET_SIZE];

    @Before
    public void initialize() {
        Random random = new Random();

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            keys[i] = Integer.toString(i * INDEX_SCALE);
            values[i] = random.nextInt();
        }
    }

    @After
    public void terminate() {
        keys = null;
        values = null;
    }

    @Test
    public void functionalTest() throws Map.NoSuchElementException {
        Map<String, Integer> map = new HashTable<>(new DefaultHasher());

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            map.set(keys[i], values[i]);
            Assert.assertTrue(map.get(keys[i]).equals(values[i]));
        }

        for (int i = ERASE_START; i <= ERASE_END; i++) {
            map.remove(keys[i]);
        }

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            if (i < ERASE_START || i > ERASE_END) {
                Assert.assertTrue(map.get(keys[i]).equals(values[i]));
            }
        }
    }

    @Test
    public void changeHasherTest() throws Map.NoSuchElementException {
        HashTable<String, Integer> table = new HashTable<>(new SDBMHasher());

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            table.set(keys[i], values[i]);
            Assert.assertTrue(table.get(keys[i]).equals(values[i]));
        }

        for (int i = ERASE_START; i <= ERASE_END; i++) {
            table.remove(keys[i]);
        }

        table.setHasher(new DJB2Hasher());

        for (int i = 0; i < TEST_SET_SIZE; i++) {
            if (i < ERASE_START || i > ERASE_END) {
                Assert.assertTrue(table.get(keys[i]).equals(values[i]));
            }
        }
    }
}
