package org.carreiro.experiments;

public class UseTrait implements SomeTrait {
    @Override
    public int code() {
        return 42;
    }

    @Override
    public String greeting() {
        return "Death to Goodell";
    }

    public static void main(String[] args) {
        UseTrait t = new UseTrait();

        System.out.println(t.code());
        System.out.println(t.greeting());
    }
}

class UseAnotherTrait implements SomeOtherTrait {
    public static void main(String[] args) {
        UseAnotherTrait t = new UseAnotherTrait();

        System.out.println(t.code());
        System.out.println(t.greeting());
    }
}
