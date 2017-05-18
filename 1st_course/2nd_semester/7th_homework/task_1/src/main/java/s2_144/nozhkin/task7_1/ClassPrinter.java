package s2_144.nozhkin.task7_1;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;

/** class that can print other classes/annotations/interfaces/enums */
public class ClassPrinter {
    /** level offset */
    private static final String TAB = "    ";

    /** class that prints modifiers of inner entities of classes */
    private static final ModifierPrinter CLASS_MODIFIER_PRINTER = new ClassModifierPrinter();

    /** class that prints modifiers of classes */
    private static final ModifierPrinter DEFAULT_MODIFIER_PRINTER = new DefaultModifierPrinter();

    /** class that prints modifiers of inner entities of interfaces */
    private static final ModifierPrinter INTERFACE_MODIFIER_PRINTER = new InterfaceModifierPrinter();

    /** class that prints modifiers of interfaces/enums/annotations */
    private static final ModifierPrinter SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER =
                                         new SpecialClassDeclarationModifierPrinter();

    /**
     * makes a list from array and removes some elements
     *
     * @param array original array
     * @param removed elements that should be removed
     * @param <T> type of elements
     * @return list contains elements without elements of array "removed"
     */
    private static <T> List<T> arrayWithout(T[] array, T... removed) {
        List<T> list = new ArrayList(Arrays.asList(array));
        list.removeAll(Arrays.asList(removed));
        return list;
    }

    /**
     * appends all strings to builder
     *
     * @param builder target
     * @param strings source
     */
    private static void multipleAppend(StringBuilder builder, String... strings) {
        for (String string : strings) {
            builder.append(string);
        }
    }

    /**
     * prints annotation in such format: @name(value)
     *
     * @param builder output builder
     * @param annotation annotation object
     */
    private static void printAnnotation(StringBuilder builder, Annotation annotation) {
        Class type = annotation.annotationType();

        multipleAppend(builder, "@", type.getSimpleName());

        Method[] methods = annotation.annotationType().getDeclaredMethods();
        if (methods.length > 0) {
            builder.append("(");

            for (int i = 0; i < methods.length; i++) {
                String value = null;
                boolean accessible = false;
                Method method = methods[i];
                try {
                    accessible = method.isAccessible();
                    method.setAccessible(true);
                    Object valueObject = method.invoke(annotation);
                    if (valueObject != null) {
                        value = valueObject.toString();
                    }
                    method.setAccessible(accessible);
                } catch (IllegalAccessException e) {
                } catch (InvocationTargetException e) {
                    method.setAccessible(accessible);
                } catch (IllegalArgumentException e) {
                    method.setAccessible(accessible);
                }

                if (value != null) {
                    builder.append(value);
                }
                if (i < methods.length - 1) {
                    builder.append(", ");
                }
            }

            builder.append(")");
        }
    }

    /**
     * prints all given annotation (each on a new line)
     *
     * @param builder output builder
     * @param annotations annotation array
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printAnnotations(StringBuilder builder, Annotation[] annotations, String offset) {
        for (Annotation annotation : annotations) {
            builder.append(offset);
            printAnnotation(builder, annotation);
            builder.append("\n");
        }
    }

    /**
     * prints value of primitive (or string) static final field
     *
     * @param field field object
     * @return string that contains value of a field
     */
    private static String printFieldValue(Field field) {
        if (!Modifier.isStatic(field.getModifiers()) || !Modifier.isFinal(field.getModifiers()) ||
                !(field.getType().isPrimitive() || field.getType() == String.class)) {
            return null;
        }

        boolean accessible = field.isAccessible();
        field.setAccessible(true);

        String value = null;
        try {
            if (field.getType() == String.class) {
                value = '"' + field.get(null).toString() + '"';
            } else {
                value = field.get(null).toString();
            }
        } catch (IllegalAccessException e) {}

        field.setAccessible(accessible);

        return value;
    }

    /**
     * prints field in such format: modifiers type name = value;
     *
     * @param builder output builder
     * @param field field object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printField(StringBuilder builder, Field field, String offset, ModifierPrinter modifierPrinter) {
        printAnnotations(builder, field.getDeclaredAnnotations(), offset + TAB);
        multipleAppend(builder, offset, TAB, modifierPrinter.printModifiers((field.getModifiers())),
                       field.getType().getSimpleName(), " ", field.getName());

        String value = printFieldValue(field);
        if (value != null) {
            multipleAppend(builder, " = ", value);
        }
    }

    /**
     * prints constructor in such format: modifiers name(arguments)
     *
     * @param builder output builder
     * @param constructor constructor object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printConstructor(StringBuilder builder, Constructor constructor,
                                         String offset, ModifierPrinter modifierPrinter) {
        printAnnotations(builder, constructor.getDeclaredAnnotations(), offset + TAB);
        multipleAppend(builder, offset, TAB, modifierPrinter.printModifiers(constructor.getModifiers()),
                       constructor.getDeclaringClass().getSimpleName(), "(");

        Parameter[] parameters = constructor.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            multipleAppend(builder, parameters[i].getType().getSimpleName(), " ", parameters[i].getName());
            if (i < parameters.length - 1) {
                builder.append(", ");
            }
        }

        builder.append(")");

        Class[] exceptions = constructor.getExceptionTypes();
        if (exceptions.length > 0) {
            builder.append(" throws ");
            for (int i = 0; i < exceptions.length; i++) {
                builder.append(exceptions[i].getTypeName());
                if (i < exceptions.length - 1) {
                    builder.append(", ");
                }
            }
        }
    }

    /**
     * prints a method in such format: modifiers returnType name(arguments)
     *
     * @param builder output builder
     * @param method method object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printMethod(StringBuilder builder, Method method, String offset,
                                      ModifierPrinter modifierPrinter) {
        printAnnotations(builder, method.getDeclaredAnnotations(), offset + TAB);
        multipleAppend(builder, offset, TAB, modifierPrinter.printModifiers(method.getModifiers()),
                       method.getReturnType().getSimpleName(), " ", method.getName(), "(");

        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            multipleAppend(builder, parameters[i].getType().getSimpleName(), " ", parameters[i].getName());
            if (i < parameters.length - 1) {
                builder.append(", ");
            }
        }

        builder.append(")");

        Class[] exceptions = method.getExceptionTypes();
        if (exceptions.length > 0) {
            builder.append(" throws ");
            for (int i = 0; i < exceptions.length; i++) {
                builder.append(exceptions[i].getTypeName());
                if (i < exceptions.length - 1) {
                    builder.append(", ");
                }
            }
        }
    }

    /**
     * prints an array of interfaces
     *
     * @param builder output builder
     * @param interfaces array of interfaces
     */
    private static void printInterfaces(StringBuilder builder, Class[] interfaces) {
        for (int i = 0; i < interfaces.length; i++) {
            builder.append(interfaces[i].getSimpleName());
            if (i < interfaces.length - 1) {
                builder.append(", ");
            }
        }
    }

    /**
     * prints an array of field (each on a new array)
     *
     * @param builder output builder
     * @param fields array of fields
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printFields(StringBuilder builder, Field[] fields,
                                    String offset, ModifierPrinter modifierPrinter) {
        for (Field field : fields) {
            printField(builder, field, offset, modifierPrinter);
            builder.append(";\n");
        }
    }

    /**
     * prints an array of constructors (each on a new line)
     *
     * @param builder output builder
     * @param constructors array of constructor
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printConstructors(StringBuilder builder, Constructor[] constructors,
                                          String offset, ModifierPrinter modifierPrinter) {
        if (constructors.length > 0) {
            builder.append('\n');
        }
        for (Constructor constructor : constructors) {
            printConstructor(builder, constructor, offset, modifierPrinter);
            builder.append(";\n");
        }
    }

    /**
     * prints an array of methods (each on a new line)
     *
     * @param builder output builder
     * @param methods array of methods
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     */
    private static void printMethods(StringBuilder builder, Method[] methods,
                                     String offset, ModifierPrinter modifierPrinter) {
        if (methods.length > 0) {
            builder.append('\n');
        }
        for (Method method : methods) {
            printMethod(builder, method, offset, modifierPrinter);
            builder.append(";\n");
        }
    }

    /**
     * prints internal declared entities
     *
     * @param builder output builder
     * @param internals array of internal declared classes
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printInternals(StringBuilder builder, Class[] internals, String offset) {
        if (internals.length > 0) {
            builder.append("\n");
        }
        for (int i = 0; i < internals.length; i++) {
            printEntity(builder, internals[i], offset + TAB);
            if (i < internals.length - 1) {
                builder.append("\n");
            }
        }
    }

    /**
     * prints a class
     *
     * @param builder output builder
     * @param entity class object of a class
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printClass(StringBuilder builder, Class entity, String offset) {
        printAnnotations(builder, entity.getDeclaredAnnotations(), offset);
        multipleAppend(builder, offset, DEFAULT_MODIFIER_PRINTER.printModifiers(entity.getModifiers()),
                       "class", " ", entity.getSimpleName(), " ");

        if (entity.getSuperclass() != Object.class) {
            multipleAppend(builder, "extends ", entity.getSuperclass().getSimpleName(), " ");
        }

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            builder.append("implements ");
            printInterfaces(builder, entity.getInterfaces());
            builder.append(" ");
        }

        builder.append("{\n");

        printFields(builder, entity.getDeclaredFields(), offset, CLASS_MODIFIER_PRINTER);
        printConstructors(builder, entity.getDeclaredConstructors(), offset, CLASS_MODIFIER_PRINTER);
        printMethods(builder, entity.getDeclaredMethods(), offset, CLASS_MODIFIER_PRINTER);
        printInternals(builder, entity.getDeclaredClasses(), offset);

        multipleAppend(builder, offset, "}", "\n");
    }

    /**
     * prints an interface
     *
     * @param builder output builder
     * @param entity class object of an interface
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printInterface(StringBuilder builder, Class entity, String offset) {
        printAnnotations(builder, entity.getDeclaredAnnotations(), offset);
        multipleAppend(builder, offset,
                       SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()),
                       "interface ", entity.getSimpleName(), " ");

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            builder.append("extends ");
            printInterfaces(builder, entity.getInterfaces());
            builder.append(" ");
        }

        builder.append("{\n");

        printMethods(builder, entity.getDeclaredMethods(), offset, INTERFACE_MODIFIER_PRINTER);
        printInternals(builder, entity.getDeclaredClasses(), offset);

        multipleAppend(builder, offset, "}", "\n");
    }

    /**
     * prints an annotation
     *
     * @param builder output builder
     * @param entity class obect of an annotation
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printAnnotationClass(StringBuilder builder, Class entity, String offset) {
        printAnnotations(builder, entity.getDeclaredAnnotations(), offset);
        multipleAppend(builder, offset,
                       SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()),
                       "@interface ", entity.getSimpleName(), " {\n");

        printMethods(builder, entity.getDeclaredMethods(), offset, INTERFACE_MODIFIER_PRINTER);
        printInternals(builder, entity.getDeclaredClasses(), offset);

        multipleAppend(builder, offset, "}", "\n");
    }

    /**
     * returns values of an enum
     *
     * @param entity class object of enum
     * @return array of enum values
     */
    private static Field[] getEnumValues(Class entity) {
        Field[] fields = entity.getDeclaredFields();

        ArrayList<Field> enumFields = new ArrayList<>();
        for (Field field : fields) {
            int modifiers = field.getModifiers();
            if (Modifier.isPublic(modifiers) && Modifier.isFinal(modifiers) && Modifier.isStatic(modifiers) &&
                    field.getType() == entity) {
                enumFields.add(field);
            }
        }

        return enumFields.toArray(new Field[0]);
    }

    /**
     * prints values of an enum
     *
     * @param builder output builder
     * @param values array of enum values
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printEnumValues(StringBuilder builder, Field[] values, String offset) {
        for (int i = 0; i < values.length; i++) {
            multipleAppend(builder, offset, TAB, values[i].getName(), i == values.length - 1 ? ";" : ",", "\n");
        }
    }

    /**
     * prints an enum
     *
     * @param builder output builder
     * @param entity class object of enum
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printEnum(StringBuilder builder, Class entity, String offset) {
        printAnnotations(builder, entity.getDeclaredAnnotations(), offset);
        multipleAppend(builder, offset,
                       SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()),
                       "enum ", entity.getSimpleName(), " ");

        if (entity.getSuperclass() != Enum.class) {
            multipleAppend(builder, "extends ", entity.getSuperclass().getSimpleName(), " ");
        }

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            builder.append("implements ");
            printInterfaces(builder, entity.getInterfaces());
            builder.append(" ");
        }

        builder.append("{\n");

        Field[] fields = getEnumValues(entity);
        try {
            fields = arrayWithout(fields, entity.getDeclaredField("$VALUES"))
                    .toArray(new Field[0]);
        } catch (NoSuchFieldException e) {}

        Field[] enumValues = getEnumValues(entity);
        fields = arrayWithout(fields, enumValues).toArray(new Field[0]);

        printEnumValues(builder, enumValues, offset);
        printFields(builder, fields, offset, CLASS_MODIFIER_PRINTER);

        Constructor[] constructors = entity.getDeclaredConstructors();
        try {
            constructors = arrayWithout(constructors, entity.getDeclaredConstructor(String.class, int.class))
                                       .toArray(new Constructor[0]);
        } catch (NoSuchMethodException e) {}
        printConstructors(builder, constructors, offset, CLASS_MODIFIER_PRINTER);

        Method[] methods = entity.getDeclaredMethods();
        try {
            methods = arrayWithout(methods, entity.getDeclaredMethod("values"),
                                            entity.getDeclaredMethod("valueOf", String.class)).toArray(new Method[0]);
        } catch (NoSuchMethodException e) {}
        printMethods(builder, methods, offset, CLASS_MODIFIER_PRINTER);

        printInternals(builder, entity.getDeclaredClasses(), offset);

        multipleAppend(builder, offset, "}", "\n");
    }

    /**
     * prints all possible entities (classes, interfaces, annotations, enums)
     *
     * @param builder output builder
     * @param entity class object of entity
     * @param offset string that should be printed at the beginning of a line
     */
    private static void printEntity(StringBuilder builder, Class entity, String offset) {
        if (entity.isAnnotation()) {
            printAnnotationClass(builder, entity, offset);
        } else if (entity.isInterface()) {
            printInterface(builder, entity, offset);
        } else if (entity.isEnum()) {
            printEnum(builder, entity, offset);
        } else {
            printClass(builder, entity, offset);
        }
    }

    /**
     * tries to print entity by its class object
     *
     * @param data class object
     * @return class that is printed to a string
     */
    public static String printClass(Class data) {
        StringBuilder builder = new StringBuilder();
        printEntity(builder, data, "");
        return builder.toString();
    }

    /** class that prints modifiers */
    private abstract static class ModifierPrinter {
        /**
         * prints modifiers to a string
         *
         * @param modifiers value that contains flags of provided modifiers
         * @return string that contatins printed modifiers
         */
        public abstract String printModifiers(int modifiers);

        /**
         * prints modifiers to a string excepting some of them
         *
         * @param modifiers value that contains flags of provided modifiers
         * @param hidden modifiers that shouldn't be printed
         * @return string that contains printed modifiers
         */
        protected String printModifiers(int modifiers, ArrayList<String> hidden) {
            StringBuilder builder = new StringBuilder();

            if (Modifier.isPublic(modifiers) && !hidden.contains("public")) {
                builder.append("public ");
            }

            if (Modifier.isPrivate(modifiers) && !hidden.contains("private")) {
                builder.append("private ");
            }

            if (Modifier.isProtected(modifiers) && !hidden.contains("protected")) {
                builder.append("protected ");
            }

            if (Modifier.isAbstract(modifiers) && !hidden.contains("abstract")) {
                builder.append("abstract ");
            }

            if (Modifier.isFinal(modifiers) && !hidden.contains("final")) {
                builder.append("final ");
            }

            if (Modifier.isNative(modifiers) && !hidden.contains("native")) {
                builder.append("native ");
            }

            if (Modifier.isStatic(modifiers) && !hidden.contains("static")) {
                builder.append("static ");
            }

            if (Modifier.isTransient(modifiers) && !hidden.contains("transient")) {
                builder.append("transient ");
            }

            if (Modifier.isVolatile(modifiers) && !hidden.contains("volatile")) {
                builder.append("volatile ");
            }

            return builder.toString();
        }
    }

    /** prints all possible modifiers */
    private static class DefaultModifierPrinter extends ModifierPrinter {
        private final ArrayList<String> HIDDEN = new ArrayList<>();

        @Override
        public String printModifiers(int modifiers) {
            return printModifiers(modifiers, HIDDEN);
        }
    }

    /** prints all possible modifiers */
    private static class ClassModifierPrinter extends ModifierPrinter {
        private final ArrayList<String> HIDDEN = new ArrayList<>();

        @Override
        public String printModifiers(int modifiers) {
            return printModifiers(modifiers, HIDDEN);
        }
    }

    /** prints modifiers excluding public and abstract */
    private static class InterfaceModifierPrinter extends ModifierPrinter {
        private final ArrayList<String> HIDDEN = new ArrayList<>(Arrays.asList("public", "abstract"));

        @Override
        public String printModifiers(int modifiers) {
            return printModifiers(modifiers, HIDDEN);
        }
    }

    /** prints modifiers excluding final and abstract */
    private static class SpecialClassDeclarationModifierPrinter extends ModifierPrinter {
        private final ArrayList<String> HIDDEN = new ArrayList<>(Arrays.asList("final", "abstract", "static"));

        @Override
        public String printModifiers(int modifiers) {
            return printModifiers(modifiers, HIDDEN);
        }
    }
}