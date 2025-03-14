class Main inherits IO {
    main() : Object {{
        if "string" < "another" then 
            out_string("1\n")
        else
            out_string("2\n")
        fi;
        if "xyz" <= "abc" then
            out_string("3\n")
        else
            out_string("4\n")
        fi;
        if "qwerty" = "not qwerty" then
            out_string("5\n")
        else
            out_string("6\n")
        fi;  
    }};
};