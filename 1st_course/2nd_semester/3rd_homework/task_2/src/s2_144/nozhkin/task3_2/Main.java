package s2_144.nozhkin.task3_2;

import java.io.File;
import java.io.IOException;
import java.util.Random;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int size = readSize(scanner);
        int[][] array = createArray(scanner, size);
        SpiralPrinter printer = createPrinter(scanner);
        print(array, printer);
    }

    private static int readSize(Scanner scanner) {
        System.out.print("Enter the size of array: ");
        int size = scanner.nextInt();
        scanner.nextLine();
        return size;
    }

    private static int[][] createArray(Scanner scanner, int size) {
        System.out.print("Do you want to enter array manually? " +
                "(If not it will be filled with random numbers) Y(y) / N(smth): ");
        String answer = scanner.nextLine();

        int[][] array = null;
        if (answer.toLowerCase().compareTo("y") == 0) {
            System.out.println("Enter your array:");
            array = readArray(scanner, size);
            scanner.nextLine();
        } else {
            array = generateArray(size);
            System.out.println("Your array is:");
            printArray(array);
        }

        return array;
    }

    private static SpiralPrinter createPrinter(Scanner scanner) {
        System.out.print("Do you want to print array to file? (If not it will be printed to console) Y(y) / N(smth): ");
        String answer = scanner.nextLine();

        SpiralPrinter printer = null;
        if (answer.toLowerCase().compareTo("y") == 0) {
            System.out.print("Enter the name of file: ");
            answer = scanner.nextLine();
            printer = new FileSpiralPrinter(new File(answer));
        } else {
            printer = new ConsoleSpiralPrinter();
        }

        return printer;
    }

    private static void print(int[][] array, SpiralPrinter printer) {
        try {
            printer.print(array);
            System.out.println();
            System.out.println("Array has been printed");
        } catch (SpiralPrinter.ArrayIsNotPrintableException e) {
            System.out.println("Your array is not printable");
        } catch (IOException e) {
            System.out.println("Couldn't write to file. " + e.getMessage());
        } catch (Exception e) {
            System.out.println("Something very awful was happened. " + e.getMessage());
        }
    }

    private static int[][] readArray(Scanner scanner, int size) {
        int array[][] = new int[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                array[i][j] = scanner.nextInt();
            }
        }

        return array;
    }

    private static int[][] generateArray(int size) {
        Random generator = new Random();

        int array[][] = new int[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                array[i][j] = Math.abs(generator.nextInt()) % 10;
            }
        }

        return array;
    }

    private static void printArray(int[][] array) {
        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < array[i].length; j++) {
                System.out.print(array[i][j]);
                System.out.print(' ');
            }
            System.out.println();
        }
    }
}
