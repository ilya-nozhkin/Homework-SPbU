package s2_144.nozhkin.task4_2;

import java.util.Scanner;

public class Main {
    private enum Operation {
        EXIT,
        HELP,
        STATISTICS,
        SET,
        GET,
        REMOVE,
        HASH,
    }

    private enum Hash {
        DJB2,
        SDBM,
        SUM,
        DEFAULT
    }

    public static void main(String[] args) {
        HashTable<String, String> table = new HashTable<>(new DefaultHasher());

	    Scanner scanner = new Scanner(System.in);

        printHelp();

        boolean process = true;
        while (process) {
            System.out.print(">");
            String command = scanner.nextLine();
            String[] splitted =  command.split(" ", 2);
            String operationString = splitted[0];

            try {
                Operation operation = Operation.valueOf(operationString.toUpperCase());

                if (!execute(table, operation, splitted)) {
                    process = false;
                }
            } catch (IllegalArgumentException e) {
                System.out.println("Unknown operation '" + operationString + "'");
            }
        }
    }

    private static boolean execute(HashTable<String, String> table, Operation operation, String[] splitResult) {
        switch (operation) {
            case EXIT:
                return false;
            case HELP:
                printHelp();
                break;
            case STATISTICS:
                printStatistics(table);
                break;
            case SET:
                if (checkArgumentsNumber(splitResult)) {
                    operationSet(table, splitResult[1]);
                }
                break;
            case GET:
                if (checkArgumentsNumber(splitResult)) {
                    operationGet(table, splitResult[1]);
                }
                break;
            case REMOVE:
                if (checkArgumentsNumber(splitResult)) {
                    operationRemove(table, splitResult[1]);
                }
                break;
            case HASH:
                if (checkArgumentsNumber(splitResult)) {
                    operationHash(table, splitResult[1]);
                }
                break;
        }
        return true;
    }

    private static void printHelp() {
        System.out.println("This program provides an interface to hash table.");
        System.out.println("Available commands:");
        System.out.println("exit - close the program");
        System.out.println("help - print this message");
        System.out.println("statistics - print the statistics");
        System.out.println("set <key>=<value> - associate <value> with <key>");
        System.out.println("get <key> - get value associated with <key>");
        System.out.println("remove <key> - remove value associated with <key>");
        System.out.println("hash <name> - change hash function to <name>, available functions:");
        System.out.println("\tdjb2, sdbm, sum, default \t ('default' is default java hash method)");
    }

    private static void printStatistics(HashTable<String, String> table) {
        Map.Statistics statistics = table.getStatistics();
        System.out.println("cells - " + statistics.cells);
        System.out.println("load factor - " + statistics.loadFactor);
        System.out.println("collisions - " + statistics.collisions);
        System.out.println("max list length - " + statistics.maxListLength);
    }

    private static boolean checkArgumentsNumber(String[] splitResult) {
        if (splitResult.length != 2) {
            System.out.println("Syntax error: no arguments, type 'help' to get usage");
        }

        return splitResult.length == 2;
    }

    private static void operationSet(HashTable<String, String> table, String arguments) {
        String regex = "(?<!\\\\)="; //split by '=' but ignore '\="
        String[] splitted = arguments.split(regex, 2);

        if (splitted.length == 2) {
            String key = splitted[0];
            String value = splitted[1];

            table.set(key, value);
        } else {
            System.out.println("Syntax error: '=' not found, type 'help' to get usage");
        }
    }

    private static void operationGet(HashTable<String, String> table, String key) {
        String value = table.get(key);
        if (value != null) {
            System.out.println(value);
        } else {
            System.out.println("Hash table doesn't contain key '" + key + "'");
        }
    }

    private static void operationRemove(HashTable<String, String> table, String key) {
        try {
            table.remove(key);
        } catch (Map.NoSuchElementException e) {
            System.out.println("Hash table doesn't contain key '" + key + "'");
        }
    }

    private static void operationHash(HashTable<String, String> table, String hashName) {
        try {
            Hash hash = Hash.valueOf(hashName.toUpperCase());

            switch (hash) {
                case DJB2:
                    table.setHasher(new DJB2Hasher());
                    break;
                case SDBM:
                    table.setHasher(new SDBMHasher());
                    break;
                case SUM:
                    table.setHasher(new SumHasher());
                    break;
                case DEFAULT:
                    table.setHasher(new DefaultHasher());
                    break;
            }
        } catch (IllegalArgumentException e) {
            System.out.println("Unknown hash function '" + hashName + "'");
        }
    }

    private static class DJB2Hasher implements Hasher<String> {
        @Override
        public int hash(String data) {
            long value = 5381;
            for (int i = 0; i < data.length(); i++) {
                value = ((value << 5) + value) + data.charAt(i);
            }

            return (int) value;
        }
    }

    private static class SDBMHasher implements Hasher<String> {
        @Override
        public int hash(String data) {
            long value = 5381;
            for (int i = 0; i < data.length(); i++) {
                value = data.charAt(i) + (value << 6) + (value << 16) - value;
            }

            return (int) value;
        }
    }

    private static class SumHasher implements Hasher<String> {
        @Override
        public int hash(String data) {
            long value = 5381;
            for (int i = 0; i < data.length(); i++) {
                value += data.charAt(i);
            }

            return (int) value;
        }
    }

    private static class DefaultHasher implements Hasher<String> {
        @Override
        public int hash(String data) {
            return data.hashCode();
        }
    }
}
