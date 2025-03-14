class A inherits IO {
    x : Int;
    x() : Int {1};
};

class Main inherits A {
    main() : Object {
        out_int(x <- 1 + x() + (new Main)@A.x()).out_string("\n")
    };
    x() : Int {2};
};