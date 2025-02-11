






class Parent {};
class Child inherits Parent {};
class Boo {};
class Foo {};

class Main inherits IO {
    x : Int <- 1;
    main() : Object{{
        x <- 1;
        if 1 = x then 
            out_string("hello!\n")
        else
            out_string("wrong!\n")
        fi;
    }};
};

class Fool inherits Parent {};