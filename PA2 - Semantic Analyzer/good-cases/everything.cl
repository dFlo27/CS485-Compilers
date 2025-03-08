(* Madness *)

(* Method inheritance check *)
class Parent {
    test(x : Int, y : String) : Int {1 + 2 + 3};
};
class Child inherits Parent {
    test (x : Int, y : String) : Int {2 + 3 + 4};
    method (no_inherits : Int) : Object {2 + 3};
};

class GrandChild inherits Child {
    x : Child <- new SELF_TYPE;
    method(x : Int) : Object {method(1)};
};

class GGrandChild inherits GrandChild {};

(* Checking each expression *)
class Main inherits IO {
    e : SELF_TYPE;
    g : Child;
    h : Parent;    
    x : Int <- {
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
        g <- new Child;
        h <- new GGrandChild;
        x <- new Int;
        a(x, z);
        h.test(x, z);
        g@Parent.test(x, z);
        h.test(x, z);
        if 1 = x then 
            out_string("hello!\n")
        else
            out_string("wrong!\n")
        fi;
        while false loop
            x <- x + 1
        pool;
        let x : String <- "hi", g : Object <- 1, h : Int in x;
        case x of 
            y : Object => y <- 1 + 1;
            z : String => out_string(z);
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
        g <- new Child;
        h <- new GGrandChild;
        x <- new Int;
        a(x, z);
        h.test(x, z);
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
    a(x : Int, y : String) : Int {1 + 2 + 3};
    b(x : Int, y : String, z : IO, a : Bool, b : Object, c : Main, d : Parent) : Object {1};
};

class A {
    a : Int;
    a(a : String) : Object {
        case a of 
            a : Int    => a + 1;
            a : String => a <- "no";
            a : Object => a <- new SELF_TYPE;
        esac
    };
};