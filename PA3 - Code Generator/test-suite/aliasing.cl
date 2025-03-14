class Integer {
    i : Int;
    new_i (n : Int): SELF_TYPE {{
        i <- n; self;
    }};
    i () : Int {i};
};

class Main inherits IO {
    x : Integer;
    y : Integer;
    main() : Object {{
        x <- (new Integer);
        y <- x;
        x.new_i(1);
        out_int(x.i());
        out_string("\n");
        out_int(y.i());
        out_string("\n");
    }};
};