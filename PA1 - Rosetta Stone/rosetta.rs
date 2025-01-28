use std::io;

struct Node {
    edges: Vec<String>,
    task: String,
}

fn update(nodes: &mut Vec<Node>, task: String, req: String) {
    for node in &mut *nodes {
        if node.task == task {
            if !req.is_empty() {
                node.edges.push(req);
            }
            return;
        }
    }
    match req.is_empty() {
        true => nodes.push(Node {edges: Vec::<String>::new(), task: task}),
        _    => nodes.push(Node {edges: vec![req], task: task})
    }
}

fn main() {
    let mut nodes: Vec<Node> = Vec::new();
    let mut s = Vec::new();
    let mut l = Vec::new();
    let mut input = io::stdin().lines();
    let mut i = 0;
    let mut j;
    while let Some(task) = input.next() {
        let Some(req) = input.next() else {break};
        let req = req.unwrap();
        update(&mut nodes, task.unwrap(), req.clone());
        update(&mut nodes, req, String::new());
    }
    while i < nodes.len() {
        if nodes[i].edges.is_empty() {
            s.push(nodes.remove(i).task);
        } else {
            i += 1;
        }
    }
    s.sort();
    while !s.is_empty() {
        let n = s.remove(0);
        l.push(n.clone());
        i = 0;
        while i < nodes.len() {
            j = 0;
            while j < nodes[i].edges.len() {
                match nodes[i].edges[j] == n {
                    true => break,
                    _ => j += 1
                }
            }
            if j == nodes[i].edges.len() {
                i += 1;
                continue;
            }
            nodes[i].edges.remove(j);
            if nodes[i].edges.is_empty() {
                s.push(nodes[i].task.clone());
                s.sort();
                nodes.remove(i);
            }
        }
    }
    if !nodes.is_empty() {
        println!("cycle");
    } else {
        for task in l {
            println!("{}", task);
        }
    }
}
