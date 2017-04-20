import sun.reflect.annotation.AnnotationType;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;

/** class that can print other classes/annotations/interfaces/enums */
class ClassPrinter {
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
     * prints annotation in such format: @name(value)
     *
     * @param annotation annotation object
     * @return annotation that is printed to a string
     */
    private static String printAnnotation(Annotation annotation) {
        Class type = annotation.annotationType();

        String value = null;
        try {
            Method valueMethod = type.getMethod("value");
            if (valueMethod != null) {
                Object valueObject = valueMethod.invoke(annotation);
                if (valueObject != null) {
                    value = valueObject.toString();
                }
            }
        } catch (NoSuchMethodException e) {
        } catch (IllegalAccessException e) {
        } catch (InvocationTargetException e) {
        }

        return "@" + type.getSimpleName() + (value == null ? "" : "(" + value + ")");
    }

    /**
     * prints all given annotation (each on a new line)
     *
     * @param annotations annotation array
     * @param offset string that should be printed at the beginning of a line
     * @return annotation array that is printed to a string
     */
    private static String printAnnotations(Annotation[] annotations, String offset) {
        String result = "";
        for (Annotation annotation : annotations) {
            result += offset + printAnnotation(annotation) + "\n";
        }
        return result;
    }

    /**
     * prints value of primitive (or string) static final field
     *
     * @param field field object
     * @return value that is printed to a string
     */
    private static String printFieldValue(Field field) {
        if (!Modifier.isStatic(field.getModifiers()) || !Modifier.isFinal(field.getModifiers()) ||
                !(field.getType().isPrimitive() || field.getType() == String.class)) {
            return null;
        }

        boolean accessible = field.isAccessible();
        field.setAccessible(true);

        try {
            if (field.getType() == String.class) {
                return '"' + field.get(null).toString() + '"';
            }
            return field.get(null).toString();
        } catch (IllegalAccessException e) {}

        field.setAccessible(accessible);

        return null;
    }

    /**
     * prints field in such format: modifiers type name = value;
     *
     * @param field field object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return field that is printed to a string
     */
    private static String printField(Field field, String offset, ModifierPrinter modifierPrinter) {
        String result = printAnnotations(field.getDeclaredAnnotations(), offset + TAB) +
                offset + TAB +
                modifierPrinter.printModifiers((field.getModifiers())) +
                field.getType().getSimpleName() + " " +
                field.getName();

        String value = printFieldValue(field);
        if (value != null) {
            result += " = " + value;
        }
        return result;
    }

    /**
     * prints constructor in such format: modifiers name(arguments)
     *
     * @param constructor constructor object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return constructor that is printed to a string
     */
    private static String printConstructor(Constructor constructor, String offset, ModifierPrinter modifierPrinter) {
        String result = printAnnotations(constructor.getDeclaredAnnotations(), offset + TAB) +
                offset + TAB +
                modifierPrinter.printModifiers(constructor.getModifiers()) +
                constructor.getDeclaringClass().getSimpleName() + "(";

        Parameter[] parameters = constructor.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            result += parameters[i].getType().getSimpleName() + " " + parameters[i].getName();
            if (i < parameters.length - 1) {
                result += ", ";
            }
        }

        result += ")";

        Class[] exceptions = constructor.getExceptionTypes();
        if (exceptions.length > 0) {
            result += " throws ";
            for (int i = 0; i < exceptions.length; i++) {
                result += exceptions[i].getTypeName();
                if (i < exceptions.length - 1) {
                    result += ", ";
                }
            }
        }

        return result;
    }

    /**
     * prints a method in such format: modifiers returnType name(arguments)
     *
     * @param method method object
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return methods that is printed to a string
     */
    private static String printMethod(Method method, String offset, ModifierPrinter modifierPrinter) {
        String result = printAnnotations(method.getDeclaredAnnotations(), offset + TAB) +
                offset + TAB +
                modifierPrinter.printModifiers(method.getModifiers()) +
                method.getReturnType().getSimpleName() + " " +
                method.getName() + "(";

        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            result += parameters[i].getType().getSimpleName() + " " + parameters[i].getName();
            if (i < parameters.length - 1) {
                result += ", ";
            }
        }

        result += ")";

        Class[] exceptions = method.getExceptionTypes();
        if (exceptions.length > 0) {
            result += " throws ";
            for (int i = 0; i < exceptions.length; i++) {
                result += exceptions[i].getTypeName();
                if (i < exceptions.length - 1) {
                    result += ", ";
                }
            }
        }

        return result;
    }

    /**
     * prints an array of interfaces
     *
     * @param interfaces array of interfaces
     * @return interfaces that are printed to a string
     */
    private static String printInterfaces(Class[] interfaces) {
        String result = "";
        for (int i = 0; i < interfaces.length; i++) {
            result += interfaces[i].getSimpleName();
            if (i < interfaces.length - 1) {
                result += ", ";
            }
        }

        return result;
    }

    /**
     * prints an array of field (each on a new array)
     *
     * @param fields array of fields
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return fields that are printed to a string
     */
    private static String printFields(Field[] fields, String offset, ModifierPrinter modifierPrinter) {
        String result = "";
        for (Field field : fields) {
            result += printField(field, offset, modifierPrinter) + ";\n";
        }
        return result;
    }

    /**
     * prints an array of constructors (each on a new line)
     *
     * @param constructors array of constructor
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return constructors that are printed to a string
     */
    private static String printConstructors(Constructor[] constructors, String offset, ModifierPrinter modifierPrinter) {
        String result = "";
        if (constructors.length > 0) {
            result += '\n';
        }
        for (Constructor constructor : constructors) {
            result += printConstructor(constructor, offset, modifierPrinter) + ";\n";
        }
        return result;
    }

    /**
     * prints an array of methods (each on a new line)
     *
     * @param methods array of methods
     * @param offset string that should be printed at the beginning of a line
     * @param modifierPrinter class that prints modifiers
     * @return methods that are printed to a string
     */
    private static String printMethods(Method[] methods, String offset, ModifierPrinter modifierPrinter) {
        String result = "";
        if (methods.length > 0) {
            result += '\n';
        }
        for (Method method : methods) {
            result += printMethod(method, offset, modifierPrinter) + ";\n";
        }
        return result;
    }

    /**
     * prints internal declared entities
     *
     * @param internals array of internal declared classes
     * @param offset string that should be printed at the beginning of a line
     * @return internals that are printed to a string
     */
    private static String printInternals(Class[] internals, String offset) {
        String result = "";
        if (internals.length > 0) {
            result += "\n";
        }
        for (int i = 0; i < internals.length; i++) {
            result += printEntity(internals[i], offset + TAB);
            if (i < internals.length - 1) {
                result += "\n";
            }
        }
        return result;
    }

    /**
     * prints a class
     *
     * @param entity class object of a class
     * @param offset string that should be printed at the beginning of a line
     * @return class that is printed to a string
     */
    private static String printClass(Class entity, String offset) {
        String result = printAnnotations(entity.getDeclaredAnnotations(), offset);
        result += offset + DEFAULT_MODIFIER_PRINTER.printModifiers(entity.getModifiers()) +
                "class " + entity.getSimpleName() + " ";

        if (entity.getSuperclass() != Object.class) {
            result += "extends " + entity.getSuperclass().getSimpleName() + " ";
        }

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            result += "implements " + printInterfaces(entity.getInterfaces()) + " ";
        }

        result += "{\n";

        result += printFields(entity.getDeclaredFields(), offset, CLASS_MODIFIER_PRINTER);
        result += printConstructors(entity.getDeclaredConstructors(), offset, CLASS_MODIFIER_PRINTER);
        result += printMethods(entity.getDeclaredMethods(), offset, CLASS_MODIFIER_PRINTER);
        result += printInternals(entity.getDeclaredClasses(), offset);

        result += offset + "}" + "\n";

        return result;
    }

    /**
     * prints an interface
     *
     * @param entity class object of an interface
     * @param offset string that should be printed at the beginning of a line
     * @return interface that is printed to a string
     */
    private static String printInterface(Class entity, String offset) {
        String result = printAnnotations(entity.getDeclaredAnnotations(), offset);
        result += offset + SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()) +
                "interface " + entity.getSimpleName() + " ";

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            result += "extends " + printInterfaces(entity.getInterfaces()) + " ";
        }

        result += "{\n";

        result += printMethods(entity.getDeclaredMethods(), offset, INTERFACE_MODIFIER_PRINTER);
        result += printInternals(entity.getDeclaredClasses(), offset);

        result += offset + "}" + "\n";

        return result;
    }

    /**
     * prints an annotation
     *
     * @param entity class obect of an annotation
     * @param offset string that should be printed at the beginning of a line
     * @return annotation that is printed to a string
     */
    private static String printAnnotationClass(Class entity, String offset) {
        String result = printAnnotations(entity.getDeclaredAnnotations(), offset);
        result += offset + SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()) +
                "@interface " + entity.getSimpleName() + " {\n";

        result += printMethods(entity.getDeclaredMethods(), offset, INTERFACE_MODIFIER_PRINTER);
        result += printInternals(entity.getDeclaredClasses(), offset);

        result += offset + "}" + "\n";

        return result;
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
     * @param values array of enum values
     * @param offset string that should be printed at the beginning of a line
     * @return values that are printed to a string
     */
    private static String printEnumValues(Field[] values, String offset) {
        String result = "";
        for (int i = 0; i < values.length; i++) {
            result += offset + TAB + values[i].getName() + (i == values.length - 1 ? ";" : ",") + "\n";
        }
        return result;
    }

    /**
     * prints an enum
     *
     * @param entity class object of enum
     * @param offset string that should be printed at the beginning of a line
     * @return enum that is printed to a string
     */
    private static String printEnum(Class entity, String offset) {
        String result = printAnnotations(entity.getDeclaredAnnotations(), offset);
        result += offset + SPECIAL_CLASS_DECLARATION_MODIFIER_PRINTER.printModifiers(entity.getModifiers()) +
                "enum " + entity.getSimpleName() + " ";

        if (entity.getSuperclass() != Enum.class) {
            result += "extends " + entity.getSuperclass().getSimpleName() + " ";
        }

        Class[] interfaces = entity.getInterfaces();
        if (interfaces.length > 0) {
            result += "implements " + printInterfaces(entity.getInterfaces()) + " ";
        }

        result += "{\n";

        Field[] fields = getEnumValues(entity);
        try {
            fields = arrayWithout(fields, entity.getDeclaredField("$VALUES"))
                    .toArray(new Field[0]);
        } catch (NoSuchFieldException e) {}

        Field[] enumValues = getEnumValues(entity);
        fields = arrayWithout(fields, enumValues).toArray(new Field[0]);

        result += printEnumValues(enumValues, offset);
        result += printFields(fields, offset, CLASS_MODIFIER_PRINTER);

        Constructor[] constructors = entity.getDeclaredConstructors();
        try {
            constructors = arrayWithout(constructors, entity.getDeclaredConstructor(String.class, int.class))
                                       .toArray(new Constructor[0]);
        } catch (NoSuchMethodException e) {}
        result += printConstructors(constructors, offset, CLASS_MODIFIER_PRINTER);

        Method[] methods = entity.getDeclaredMethods();
        try {
            methods = arrayWithout(methods, entity.getDeclaredMethod("values"),
                                            entity.getDeclaredMethod("valueOf", String.class)).toArray(new Method[0]);
        } catch (NoSuchMethodException e) {}
        result += printMethods(methods, offset, CLASS_MODIFIER_PRINTER);

        result += printInternals(entity.getDeclaredClasses(), offset);

        result += offset + "}" + "\n";

        return result;
    }

    /**
     * prints all possible entities (classes, interfaces, annotations, enums)
     *
     * @param entity class object of entity
     * @param offset string that should be printed at the beginning of a line
     * @return entity that is printed to a string
     */
    private static String printEntity(Class entity, String offset) {
        if (entity.isAnnotation()) {
            return printAnnotationClass(entity, offset);
        } else if (entity.isInterface()) {
            return printInterface(entity, offset);
        } else if (entity.isEnum()) {
            return printEnum(entity, offset);
        }

        return printClass(entity, offset);
    }

    /**
     * tries to print entity by its class object
     *
     * @param data class object
     * @return class that is printed to a string
     */
    public static String printClass(Class data) {
        return printEntity(data, "");
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
            String result = "";

            if (Modifier.isPublic(modifiers) && !hidden.contains("public")) {
                result += "public ";
            }

            if (Modifier.isPrivate(modifiers) && !hidden.contains("private")) {
                result += "private ";
            }

            if (Modifier.isProtected(modifiers) && !hidden.contains("protected")) {
                result += "protected ";
            }

            if (Modifier.isAbstract(modifiers) && !hidden.contains("abstract")) {
                result += "abstract ";
            }

            if (Modifier.isFinal(modifiers) && !hidden.contains("final")) {
                result += "final ";
            }

            if (Modifier.isNative(modifiers) && !hidden.contains("native")) {
                result += "native ";
            }

            if (Modifier.isStatic(modifiers) && !hidden.contains("static")) {
                result += "static ";
            }

            if (Modifier.isTransient(modifiers) && !hidden.contains("transient")) {
                result += "transient ";
            }

            if (Modifier.isVolatile(modifiers) && !hidden.contains("volatile")) {
                result += "volatile ";
            }

            return result;
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