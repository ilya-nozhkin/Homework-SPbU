package s2_144.nozhkin.task4_2;

public class DJB2Hasher implements Hasher<String> {
    @Override
    public int hash(String data) {
        long value = 5381;
        for (int i = 0; i < data.length(); i++) {
            value = ((value << 5) + value) + data.charAt(i);
        }

        return Math.abs((int) value);
    }
}
