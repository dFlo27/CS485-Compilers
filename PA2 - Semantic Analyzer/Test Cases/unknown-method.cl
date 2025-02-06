class Foo {
    supersecretfunction () : Object {"secret stuff"};
};

class Main inherits IO {
    main() : Object {   
        out_string(supersecretfunction())
    };
};