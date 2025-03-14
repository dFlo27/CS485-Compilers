class Main inherits IO {
    bool : Bool;
    io   : IO;
    int  : Int;
    obj  : Object;
    str  : String;
    x : A;
    y : A <- new A;
    main() : Object {{
        out_string("Void Bool\n");
        if isvoid bool then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("Void IO\n");
        if isvoid io then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("Void Int\n");
        if isvoid int then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("Void Object\n");
        if isvoid obj then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("Void String\n");
        if isvoid str then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("Void A\n");
        if isvoid x then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
        out_string("new A\n");
        if isvoid y then
            out_string("yes\n")
        else
            out_string("no\n")
        fi;
    }};
};

class A {};