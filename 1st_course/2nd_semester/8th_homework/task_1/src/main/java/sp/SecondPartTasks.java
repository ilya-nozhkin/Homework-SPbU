package sp;

import javafx.util.Pair;

import java.io.*;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.function.DoubleFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class SecondPartTasks {

    private SecondPartTasks() {}

    // Найти строки из переданных файлов, в которых встречается указанная подстрока.
    public static List<String> findQuotes(List<String> paths, CharSequence sequence) {
        return paths.stream().flatMap(path -> {
                try {
                    BufferedReader input = new BufferedReader(new FileReader(new File(path)));

                    Stream<String> lines = input.lines()
                            .filter(line -> line.contains(sequence))
                            .collect(Collectors.toList())
                            .stream();

                    input.close();
                    return lines;
                } catch (IOException e) {}
                return Stream.empty();
            }).collect(Collectors.toList());
    }

    // В квадрат с длиной стороны 1 вписана мишень.
    // Стрелок атакует мишень и каждый раз попадает в произвольную точку квадрата.
    // Надо промоделировать этот процесс с помощью класса java.util.Random и посчитать, какова вероятность попасть в мишень.
    public static double piDividedBy4() {
        final int count = 2*1024*1024;
        return (double) (new Random()).doubles().mapToObj(new DoubleFunction<Pair<Double, Double>>() {
                    double previous = 0;
                    boolean state = true;

                    @Override
                    public Pair<Double, Double> apply(double value) {
                        state = !state;
                        if (state) {
                            return new Pair(previous, value);
                        }

                        previous = value;
                        return new Pair(null, null);
                    }
                })
                .filter(pair -> pair.getKey() != null)
                .limit(count)
                .filter(pair -> Math.pow(pair.getKey() - 0.5, 2) +
                                Math.pow(pair.getValue() - 0.5, 2) < 0.25)
                .count() / count;
    }

    // Дано отображение из имени автора в список с содержанием его произведений.
    // Надо вычислить, чья общая длина произведений наибольшая.
    public static String findPrinter(Map<String, List<String>> compositions) {
        return compositions.entrySet().stream()
                .map(entry -> new Pair<>(entry.getKey(),
                                         entry.getValue().stream()
                                                         .mapToLong(String::length)
                                                         .sum()))
                .max(Comparator.comparing(Pair::getValue))
                .orElse(new Pair<>(null, null)).getKey();
    }

    // Вы крупный поставщик продуктов. Каждая торговая сеть делает вам заказ в виде Map<Товар, Количество>.
    // Необходимо вычислить, какой товар и в каком количестве надо поставить.
    public static Map<String, Integer> calculateGlobalOrder(List<Map<String, Integer>> orders) {
        return orders.stream()
                .flatMap(order -> order.entrySet().stream())
                .collect(Collectors.groupingBy(Map.Entry::getKey,
                         Collectors.collectingAndThen(Collectors.toList(),
                                 list -> list.stream()
                                             .mapToInt(Map.Entry::getValue)
                                             .sum())));
    }
}