class Main {
    x : Integer;
    main() : Object {{
        if not isvoid x then
            x <- new Integer 
        else
            x <- new Double
        fi;
        x.init().out();
    }};
};

class Integer inherits IO {
    x : Int;
    x()    : Int {x};
    init() : SELF_TYPE {{x <- 1; self;}};
    out()  : SELF_TYPE {out_int(x).out_string("\n")}; 
};

class Double inherits Integer {
    f : Int;
    init() : SELF_TYPE {{f <- self@Integer.init().x() + x; self;}};
    out()  : SELF_TYPE {out_int(x).out_string(" ").out_int(f).out_string("\n")};
};