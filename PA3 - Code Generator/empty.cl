class Main {
    counter : Counter <- (new Counter).init(10);
    main() : Object {
        while not counter.is_zero() loop
            counter.decrement().accumulate(2)
        pool
    };
};

class Counter {
    count : Int;
    acc   : Int;
    init (start : Int) : SELF_TYPE {{
        count <- start; 
        self;
    }};
    decrement() : SELF_TYPE {{
        count <- count - 1; 
        self;
    }};
    accumulate(x : Int) : SELF_TYPE {{
        acc + x;
        self;
    }};
    is_zero() : Bool {count = 0};
};