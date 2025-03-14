class Main {
    x : A <- new A;
    main() : Object {{
        copy();
        x.foo();
        x <- new B;
        x.foo();
        x@A.foo();
    }};
};


class A {
    foo () : Object {"do something"};
};
class B inherits A {};