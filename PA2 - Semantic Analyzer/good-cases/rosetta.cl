class Node {
    task : String;
    next : Node;

    init (x : String) : SELF_TYPE {{
        task <- x;
        self;
    }};
    
    swap (x : Node) : SELF_TYPE  {
        let t : String <- task in {
            task <- x.get_task();
            x.set_task(t);
            self;
    }};

    get_from () : String { "" };
    get_task () : String { task };
    out      () : String { task };
    get_next () : Node   { next };
    set_next (x : Node) : Node { next <- x };
    set_task (x : String) : String { task <- x };
    set_from (x : String) : String { "" };
    equal    (x : Node) : Bool { task = x.get_task() };
    lt_eq    (x : Node) : Bool { task <= x.get_task()};
};

class Edge inherits Node {
    from : String;

    out     () : String { from.concat(", ").concat(task) };
    get_from () : String { from };
    set_from  (x : String) : String { from <- x };

    create (x : String, y : String) : SELF_TYPE {{
            from <- y;
            self.init(x);
    }};

    equal (x : Node) : Bool {
        if from = x.get_from() then
            self@Node.equal(x)
        else false fi
    };
};

class List inherits IO {
    hd : Node;
    tl : Node;

    init () : SELF_TYPE {{
        self;
    }};

    empty    () : Bool { isvoid hd };
    get_head () : Node { hd };
    get_tail () : Node { tl };

    append (node : Node) : SELF_TYPE {{
        if self.empty() then {
                hd <- node;
                tl <- node;
        } else
            tl <- tl.set_next(node)
        fi;
        self;
    }};

    attach (lst : List) : SELF_TYPE {{
        if not lst.empty() then
            while not isvoid lst.get_head() loop
                self.append(lst.pop())
            pool
        else "place-holder" fi;
        self;
    }};

    pop () : Node { 
        let t : Node <- hd in {
            hd <- hd.get_next();
            if isvoid hd then 
                tl <- hd
            else "place-holder" fi;
            t;
     }};

    (*Assumes that the node exists in list*)
     remove (r : Node) : Node {
        let iter : Node <- hd in
        if hd = r then 
            pop()
        else {
            while not iter.get_next() = r loop
                iter <- iter.get_next()
            pool;
            if iter.get_next() = tl then {
                tl <- iter;
                let e : Node in tl.set_next(e);
            }
            else iter.set_next(iter.get_next().get_next()) fi;
            r;
        } fi
     };

    display () : Object {
    let iter : Node <- hd in
        while not isvoid iter loop {
            out_string(iter.out().concat("\n"));
            iter <- iter.get_next();
        } pool
    };

    sort () : SELF_TYPE {{
        let node : Node <- hd in
            while not isvoid node loop {
            let iter : Node <- node.get_next() in
                while not isvoid iter loop {
                    if iter.lt_eq(node) then {
                        node.swap(iter);
                    } else "place-holder" fi;
                    iter <- iter.get_next();
                } pool;
                node <- node.get_next();
            } pool;
        self;
    }};
};

class Set inherits List {
    append (node : Node) : SELF_TYPE {{
        if self.empty() then
            self@List.append(node)
        else let iter : Node <- hd in {
            while if isvoid iter then false else not iter.equal(node) fi loop 
                iter <- iter.get_next()
            pool;
            if isvoid iter then
                self@List.append(node)
            else self fi;
        } fi;
    }};
};

class Main inherits IO {
    edges : List  <- (new List).init();
    nodes : Set   <- (new  Set).init();
    l     : List  <- (new List).init();
    s     : List;
    task  : String <- new String;

    unique (nodes : Set, edges : List) : List {
        let dst : List <- (new List).init(), n_iter : Node <- nodes.get_head() in {
            while not isvoid n_iter loop
            let e_iter : Node <- edges.get_head(), leaf : Bool <- true in {
                while if leaf then not isvoid e_iter else false fi loop 
                    if n_iter.get_task() = e_iter.get_task() then 
                        leaf <- false
                    else
                        e_iter <- e_iter.get_next()
                    fi
                pool;
                if leaf then
                    dst.append((new Node).init(n_iter.get_task()))
                else "place-holder" fi;
                n_iter <- n_iter.get_next();
            } pool;
        dst;
    }};

    remove_assoc (n : Node, edges : List) : Set {
        let dst : Set <- (new Set).init(), iter : Node <- edges.get_head() in {
        while not isvoid iter loop {
            if n.get_task() = iter.get_from() then
                dst.append((new Node).init(edges.remove(iter).get_task()))
            else "place-holder" fi;
            iter <- iter.get_next();
        } pool;
        dst;
    }};

    main () : Object {{
        while {task <- in_string(); not task = "";} loop
            let req : String <- in_string() in {
                edges.append((new Edge).create(task, req));
                nodes.append((new Node).init(task));
                nodes.append((new Node).init(req));
        } pool;
        nodes.sort();
        s <- unique(nodes, edges);
        while not s.empty() loop
        let n : Node <- s.pop() in {
            l.append(n);
            s.attach(unique(remove_assoc(n, edges), edges));
            s.sort();
        } pool;
        if edges.empty() then
            l.display()
        else out_string("cycle\n") fi;
    }};
};