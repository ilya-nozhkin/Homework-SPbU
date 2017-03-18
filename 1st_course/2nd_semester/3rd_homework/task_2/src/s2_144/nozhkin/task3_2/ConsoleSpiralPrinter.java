package s2_144.nozhkin.task3_2;

public class ConsoleSpiralPrinter extends SpiralConverter {
    @Override
    protected void write(int data) {
        System.out.print(data);
        System.out.print(' ');
    }
}
