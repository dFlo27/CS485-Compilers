class Main {
    x : Int <- 0;
    main() : Object {
        foo(x) = x
    };
    foo (x : Int) : Int {
        x <- 1
    };
};