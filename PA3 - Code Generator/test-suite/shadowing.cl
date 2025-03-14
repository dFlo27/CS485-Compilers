class Main inherits IO {
    x : Int <- 1;
    main() : Object {{
        out_int(x).out_string("\n");
        let x : String <- "Hello World\n" in
        out_string(x);
        test(x);
        out_int(x).out_string("\n");
        case x of 
            x : String => out_string(x <- "hello\n");
        esac;
    }};
    test(x : Int) : SELF_TYPE {
        out_int(x <- 52).out_string("\n")
    };
    
};