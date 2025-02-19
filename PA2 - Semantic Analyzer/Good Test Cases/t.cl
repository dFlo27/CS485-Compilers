






class Parent {};
class Child inherits Parent {};
class Boo inherits Child {};
class Foo inherits Boo {};

class Main inherits IO {    
    x : Int;
    y : Int;
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
class Ew {};