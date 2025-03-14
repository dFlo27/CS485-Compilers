class Main inherits IO {
    x : Object <- new Object;
    y : Object <- new Object;
    main() : Object {{
        if x = y then
            out_string("true\n")
        else
            out_string("false\n")
        fi;
        if x <= y then
            out_string("true\n")
        else
            out_string("false\n")
        fi;
        if x < y then
            out_string("true\n")
        else
            out_string("false\n")
        fi;
    }};
};