package s2_144.nozhkin.task4_2;

public class SumHasher implements Hasher<String> {
    @Override
    public int hash(String data) {
        long value = 5381;
        for (int i = 0; i < data.length(); i++) {
            value += data.charAt(i);
        }

        return Math.abs((int) value);
    }
}
