class Main inherits IO {
    x : Object;
    y : Object;
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