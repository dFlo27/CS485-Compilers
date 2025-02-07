class Foo {
    f() : SELF_TYPE {self};
};

class Main {
    main() : Object {
        let x : Foo, y : IO in
            y <- x.f()
    };
};