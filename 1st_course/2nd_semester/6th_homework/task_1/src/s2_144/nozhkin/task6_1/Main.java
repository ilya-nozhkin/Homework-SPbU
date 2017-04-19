package s2_144.nozhkin.task6_1;

import java.io.*;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
	    Scanner scanner = new Scanner(System.in);

	    System.out.print("Enter the input file name: ");
	    String inputName = scanner.nextLine();

        Node tree = readTree(inputName);

        if (tree != null) {
            printResults(tree);
        }
    }

    /**
     * builds a tree by expression that contains in file with "fileName"
     *
     * @param fileName name of file that contains the expression
     * @return root node of the tree
     */
    private static Node readTree(String fileName) {
        try(FileInputStream stream = new FileInputStream(new File(fileName))) {
            return TreeBuilder.buildTree(stream);
        } catch (IOException e) {
            System.out.println("Input file not found or it can't be read");
            return null;
        } catch (TreeBuilder.IllegalExpressionException e) {
            System.out.println("Input file contains illegal expression");
            return null;
        }
    }

    /**
     * prints an expression restructured by tree and it's result
     *
     * @param tree root node of the tree
     */
    private static void printResults(Node tree) {
        try {
            tree.print(System.out);
        } catch (IOException e) {
            System.out.print("System.out is not writable: ");
        }
        System.out.println(" = " + tree.evaluate());
    }
}
