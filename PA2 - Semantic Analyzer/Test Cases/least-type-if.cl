class Main {
    x : IO;
    main() : Object {
        x <- {
        if 1 = 1 then
            new Foo
        else
            new IO
        fi;
        }
    };
};

class Foo inherits IO {};