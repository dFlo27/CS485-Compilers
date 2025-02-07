class Main {
    main() : Object {
        let x : Foo <- new Boo in
            x.foo()
    };
};

class Foo {};
class Boo inherits Foo {
    foo() : Object{1};
};