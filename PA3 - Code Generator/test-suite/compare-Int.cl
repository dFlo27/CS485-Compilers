class Main {
    x : Bool;
    main() : Object {{
        x <- 1 < 2;
        x <- 2 < 2;
        x <- 2 < 1;
        x <- 1 <= 2;
        x <- 2 <= 2;
        x <- 3 <= 3;
        x <- 1 = 2;
        x <- 2 = 2;
        x <- 3 = 2;
    }};
}