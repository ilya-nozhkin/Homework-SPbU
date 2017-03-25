package s2_144.nozhkin.task4_2;

public class DefaultHasher implements Hasher<String> {
    @Override
    public int hash(String data) {
        return data.hashCode();
    }
}
