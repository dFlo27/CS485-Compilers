class One {
    one   () : Object {1};
    two   (one : Int) : Object {2};
    three (two : Int, one : Int) : Object {3};
    four  (three: Int, two : Int, one : Int) : Object {4};
    main() : Object {1};
};

class Two inherits One {
    one   () : Object {2};
    two   (one : Int) : Object {4};
    three (two : Int, one : Int) : Object {6};
    four  (three: Int, two : Int, one : Int) : Object {8};
    main() : Object {1};
};

class Main inherits Two {

};