
class Parent {
    test(x : Int, y : String) : Int {1 + 2 + 3};
};
class Child inherits Parent {
    test (x : Int, y : String) : Int {2 + 3 + 4};
};

class GrandChild inherits Child {};

class GGrandChild inherits GrandChild {};

class Main inherits IO {
    e : SELF_TYPE;
    f : Ew;
    g : Child;
    h : Parent;    
    x : Int <- {1;
        ~1;
        "1";
        true;
        false;
        1 + 1;
        1 - 1;
        1 / 1;
        1 * 1;
        isvoid x;
        isvoid new SELF_TYPE;
        not true;
        true < false;
        false = true;
        true <= true;
        e <- new SELF_TYPE;
        f <- (new Ew);
        g <- new Child;
        h <- new GGrandChild;
        x <- new Int;
        test(x, z);
        f.test(x, z);
        g@Parent.test(x, z);
        h.test(x, z);
        if 1 = x then 
            out_string("hello!\n")
        else
            out_string("wrong!\n")
        fi;
        while 1 = x loop
            x <- x + 1
        pool;
        let x : String <- "hi", g : Int <- 1, h : Int in x;
        case x of 
            y : Int => y;
            z : String => z;
        esac;
        copy();
        type_name();
        abort();
        1;
    };
    y : Int <- 1 + 2;
    z : String;
    main() : Object{{
        1;
        ~1;
        "1";
        true;
        false;
        1 + 1;
        1 - 1;
        1 / 1;
        1 * 1;
        isvoid x;
        isvoid new SELF_TYPE;
        not true;
        true < false;
        false = true;
        true <= true;
        e <- new SELF_TYPE;
        f <- (new Ew);
        g <- new Child;
        h <- new GGrandChild;
        x <- new Int;
        test(x, z);
        f.test(x, z);
        g@Parent.test(x, z);
        h.test(x, z);
        if 1 = x then 
            out_string("hello!\n")
        else
            out_string("wrong!\n")
        fi;
        while 1 = x loop
            x <- x + 1
        pool;
        let x : String <- "hi", g : Int <- 1, h : Int in x;
        case x of 
            y : Int => y;
            z : String => z;
        esac;
        copy();
        type_name();
        abort();
    }};
    test(x : Int, y : String) : Int {1 + 2 + 3};
    fake(x : Int, y : String, z : IO, a : Bool, b : Object, c : Main, d : Parent) : Object {1};
};

class Fool inherits Main {};

class Ew {
    test(x : Int, y : String) : Int {1 + 2 + 3};
};