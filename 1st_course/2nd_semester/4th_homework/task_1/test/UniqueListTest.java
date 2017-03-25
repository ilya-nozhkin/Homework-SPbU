import org.junit.Assert;
import org.junit.Test;
import s2_144.nozhkin.task4_1.UniqueList;

public class UniqueListTest {
    private static final int[] TEST_SET = {1, 5, 6, 2, 3, 7, 4};

    @Test
    public void functionalTest() throws UniqueList.AlreadyExistsException, UniqueList.NoSuchElementException {
        UniqueList<Integer> set = new UniqueList<>();

        for (int data : TEST_SET) {
            set.add(data);
        }

        for (int data : TEST_SET) {
            Assert.assertTrue(set.exists(data));
        }

        set.remove(TEST_SET[0]);

        Assert.assertFalse(set.exists(TEST_SET[0]));
        for (int i = 1; i < TEST_SET.length; i++) {
            Assert.assertTrue(set.exists(TEST_SET[i]));
        }
    }

    @Test(expected = UniqueList.AlreadyExistsException.class)
    public void additionTest() throws UniqueList.AlreadyExistsException {
        UniqueList<Integer> set = new UniqueList<>();

        for (int data : TEST_SET) {
            set.add(data);
        }

        set.add(TEST_SET[0]);
    }

    @Test(expected = UniqueList.NoSuchElementException.class)
    public void removingTest() throws UniqueList.NoSuchElementException, UniqueList.AlreadyExistsException {
        UniqueList<Integer> set = new UniqueList<>();

        for (int data : TEST_SET) {
            set.add(data);
        }

        for (int data : TEST_SET) {
            set.remove(data);
        }

        set.remove(TEST_SET[0]);
    }
}
