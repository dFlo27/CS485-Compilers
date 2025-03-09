class Main inherits IO {
    x : SELF_TYPE;
    main() : Object {
        x <- self@Object.copy()
    };
};

class A inherits Main {};