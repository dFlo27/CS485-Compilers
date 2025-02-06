class Chicken inherits Egg {
    x : Int;
};

class Egg inherits Chicken {
    y : Int;
};

class Main inherits IO {
    main() : Object {
        out_string("Who came first?\n\nThe chicken or the egg?\n")
    };
};