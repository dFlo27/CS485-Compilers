class Main {
    main () : Object {1};
};

class Foo inherits Main {
    x : Foo <- new Main;
};