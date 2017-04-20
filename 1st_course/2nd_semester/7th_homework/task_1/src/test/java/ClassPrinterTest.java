import org.junit.Test;
import s2_144.nozhkin.task7_1.ClassPrinter;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import static org.junit.Assert.*;

/** just a simple interface */
interface TestInterface {
    void testMethod(int arg0);
}

/** just a simple annotation */
@Retention(RetentionPolicy.RUNTIME)
@interface TestAnnotation {
    int value();
}

/** tests for ClassPrinter */
public class ClassPrinterTest {

    /** checks the interface printing */
    @Test
    public void interfaceTest() {
        final String expected =
                "interface TestInterface {\n\n" +
                "    void testMethod(int arg0);\n" +
                "}\n";

        assertTrue(ClassPrinter.printClass(TestInterface.class).equals(expected));
    }

    /** interface that extends another one */
    private interface ExtendedTestInterface extends TestInterface {
        void additionalMethod(String arg0);
    }

    /** checks the interface printing */
    @Test
    public void extendedInterfaceTest() {
        final String expected =
                "private interface ExtendedTestInterface extends TestInterface {\n\n" +
                "    void additionalMethod(String arg0);\n" +
                "}\n";

        assertTrue(ClassPrinter.printClass(ExtendedTestInterface.class).equals(expected));
    }

    /** checks the annotation printing */
    @Test
    public void annotationTest() {
        final String expected =
                "@Retention(RUNTIME)\n" +
                "@interface TestAnnotation {\n\n" +
                "    int value();\n" +
                "}\n";

        assertTrue(ClassPrinter.printClass(TestAnnotation.class).equals(expected));
    }

    /** annotated class */
    @TestAnnotation(10)
    private static class Annotated {
        public Annotated() {};
    }

    /** checks the printing of annotated entities */
    @Test
    public void annotatedTest() {
        final String expected =
                "@TestAnnotation(10)\n" +
                "private static class Annotated {\n\n" +
                "    public Annotated();\n" +
                "}\n";
        System.out.println(ClassPrinter.printClass(Annotated.class));
        assertTrue(ClassPrinter.printClass(Annotated.class).equals(expected));
    }

    /** just a simple enum */
    private enum TestEnum {
        VALUE1,
        VALUE2,
        VALUE3;
    }

    /** checks the enum printing */
    @Test
    public void enumTest() {
        final String expected =
                "private enum TestEnum {\n" +
                        "    VALUE1,\n" +
                        "    VALUE2,\n" +
                        "    VALUE3;\n" +
                        "}\n";

        assertTrue(ClassPrinter.printClass(TestEnum.class).equals(expected));
    }

    /** class that extends another class, implements some interfaces and contatins different fields, methods ans so on*/
    private static class TestClass extends Annotated implements TestInterface, ExtendedTestInterface {
        @TestAnnotation(20)
        private final static String STRING_CONSTANT = "String constant";
        protected int protectedField;

        @TestAnnotation(30)
        public TestClass() {}

        @TestAnnotation(40)
        public void testMethod(int arg0) {}
        public void additionalMethod(String arg0) {}
    }

    /**
     * checks class printing (that contains constructors, methods, fields (constants too), extends another class and
     * implements different interfaces)
     */
    @Test
    public void classTest() {
        final String expected =
                "private static class TestClass extends Annotated implements TestInterface, ExtendedTestInterface {\n" +
                "    @TestAnnotation(20)\n" +
                "    private final static String STRING_CONSTANT = \"String constant\";\n" +
                "    protected int protectedField;\n" +
                "\n" +
                "    @TestAnnotation(30)\n" +
                "    public TestClass();\n" +
                "\n" +
                "    @TestAnnotation(40)\n" +
                "    public void testMethod(int arg0);\n" +
                "    public void additionalMethod(String arg0);\n" +
                "}\n";

        assertTrue(ClassPrinter.printClass(TestClass.class).equals(expected));
    }

    /** class with internal entity */
    private static class ClassWithInternalEntities {
        public ClassWithInternalEntities() {};

        private enum InternalEnum {
            VALUE1,
            VALUE2;
        }
    }

    /** checks internal entities printing */
    @Test
    public void internalsTest() {
        final String expected =
                "private static class ClassWithInternalEntities {\n\n" +
                "    public ClassWithInternalEntities();\n" +
                "\n" +
                "    private enum InternalEnum {\n" +
                "        VALUE1,\n" +
                "        VALUE2;\n" +
                "    }\n" +
                "}\n";

        assertTrue(ClassPrinter.printClass(ClassWithInternalEntities.class).equals(expected));
    }
}
