class Main inherits Jim {
    main() : Object {1};
};

class Foo inherits Main {};
class Bin inherits Foo {};
class Fork inherits Bin {};
class Jim inherits Fork {};