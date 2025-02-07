class Main inherits IO {
    x : String <- "original";

    main() : Object {{
        let x : String <- "new" in
            out_string(x);
        out_string("\n");
        out_string(x);    
    }};
};