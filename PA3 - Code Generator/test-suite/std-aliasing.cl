class Main inherits IO {
    x : Int;
    y : Int;
    main() : Object {{
        x <- (new Int);
        y <- x + 1;
        out_int(x);
        out_string("\n");
        out_int(y);
        out_string("\n");
        let x : Int <- 10 in out_int(x).out_string("\n").out_int(y).out_string("\n");
    }};
};