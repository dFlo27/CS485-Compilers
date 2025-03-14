class Main {
    x : String;
    y : String;
    main() : Object {{
        y <- "hi".concat(x);
        x <- y;
    }};
};