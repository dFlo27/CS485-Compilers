class Main inherits IO {
    main() : Object {{
        if true < false then
            out_string("1\n")
        else
            out_string("2\n")
        fi;
        if false < true then
            out_string("3\n")
        else
            out_string("4\n")
        fi;
    }};
};