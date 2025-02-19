open Printf

(* Prints a string list in the format: [a1, a2, ..., an]\n
    Used to print the inheritance cycle error             *)
let rec print_cycle cycle =
    match cycle with
    | [] -> printf "]\n"; exit 1
    | head::tail -> if tail = [] 
        then printf "%s"   head
        else printf "%s, " head;
        print_cycle tail

(*  Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

(*  Checks whether a node has no incoming edge. Useful for calculating what task/class should go to list S (called s in this implementation) *)
let is_source lst inc = List.find_all (fun ele -> List.nth ele 1 = inc) lst = []

(* Received from a dream - https://stackoverflow.com/questions/30634119/ocaml-removing-duplicates-from-a-list-while-maintaining-order-from-the-right *)
(* Removes all duplicates, keeping the leftmost elements in the list *)
let uniq_cons lst ele = if List.mem ele lst then lst else ele :: lst
let remove_from_left lst = List.rev (List.fold_left uniq_cons [] lst)


(*  Performs Kahn's Algorithm as shown from wikipedia; 
    Returns list of sorted strings or will cry Error if a cycle is found *)
let rec inner_topo_sort s edges = 
    match s with
    | [] -> if List.is_empty edges                                                  (* We check if there are any edges left for cycle special case *)
                then []                                                             (* If edges empty; don't raise error *)
                else let () = printf "ERROR: 0: Type-Check: inheritance cycle [" in (* Else, print error and exit *)
                    remove_from_left List.(List.map (fun ele -> List.nth ele 0) edges) |> print_cycle 
    | head::tail -> 
        let m = List.find_all (fun ele -> List.hd ele = head) edges in              (* m is the list of all nodes with one incoming node equal to head *)
            let edges = List.filter (fun edge -> not (List.mem edge m)) edges in    (* We filter m out of list edges *)
                head :: inner_topo_sort (List.fast_sort String.compare ((List.filter (is_source edges) (List.map (fun edge -> List.nth edge 1) m)) @ tail)) edges
                (* ^ This line here adds m to s IF they are a source; THEN sorts new s using string comparison *)
                    
(*  Takes a string list list and returns a topo-sorted string list *)
let topo_sort edges =
    let nodes = remove_from_left (List.flatten edges) in                                (* Effectively a set containing all nodes in edges *)
        let s = List.fast_sort String.compare (List.filter (is_source edges) nodes) in  (* List s stores all sources in nodes *)
            inner_topo_sort s edges                                                     (* Function call to Kahn's algorithm *)
(* -------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Data Structures - Used to store the AST while type checking *)

type ast_attr = {
    attr_name : string;
    attr_init : string;
    attr_loc  : string;
    type_loc : string;
    attr_type : string;
}

type ast_method = {name : string}

type ast_class = {
    name            : string;
    ic_loc          : int;
    loc             : string;
    inherits        : string list;
    features_length : int;
    attributes      : ast_attr list;
    methods         : (string, string) Hashtbl.t;
}

type ast_type = {
    classes : ast_class list;
    length  : int;
}

(* First Traversal - Topological Sort the classe and try to catch class type errors *)
let rec class_type_check lst og = 
    match lst with
    | [] -> ()
    | head::tail -> 
        if List.exists (fun c -> List.hd head = c) ["Bool"; "Int"; "String"] then 
            let () = printf "ERROR: 0: Type-Check: class %s inherits from %s\n" (List.nth head 1) (List.nth head 0) in exit 1
        else if List.exists (fun c -> List.nth head 1 = List.nth c 1) tail then 
            let () = printf "ERROR: 0: Type-Check: class %s redefined\n" (List.nth head 1) in exit 1
        else if List.exists (fun c -> if List.hd head = "" then true else List.hd head = List.nth c 1) og |> not then
            let () = printf "ERROR: 0: Type-Check: class %s inherits from unknown class %s" (List.nth head 1) (List.nth head 0) in exit 1
        else 
            class_type_check tail og

let rec read_class ic name =
    match input_line ic with
    | "no_inherits" -> ["Object"; name] :: read_class ic ""
    | "inherits"    -> 
        let _ = input_line ic in 
        let parent = input_line ic in 
            [parent; name] :: read_class ic ""
    | line -> read_class ic line
    | exception End_of_file -> [[""; "Object"]; ["Object"; "Bool"]; ["Object"; "Int"]; ["Object"; "IO"]; ["Object"; "String"]];;

(* We first read ast for classes, check classes, then sort classes *)
let retrieve_class_list ic = 
    let classes = read_class ic "" in
    let () = class_type_check classes classes in
        close_in ic;
        topo_sort classes;;

let rec skip ic = 
    let return = pos_in ic in
    let str = input_line ic in
    if str = "attribute_init" || str = "attribute_no_init" || str = "method" then
        let () = seek_in ic return in ic;
    else
        skip ic

let rec new_attr_list ic = 
    let attr_init   = input_line ic in
    let attr_loc    = input_line ic in
    let attr_name   = input_line ic in
    let type_loc    = input_line ic in
    let attr_type   = input_line ic in
    let ic = skip ic in
    let return = pos_in ic in
    let attr = { 
        attr_name = attr_name;
        attr_init = attr_init;
        attr_loc = attr_loc;
        type_loc = type_loc;
        attr_type = attr_type } in
    match input_line ic with
    | "method" -> seek_in ic return; [attr]
    | line     -> seek_in ic return;  attr :: new_attr_list ic
    | exception End_of_file -> [attr]

let rec find_dup_attr class_name list =
    match list with
    | [] -> ()
    | head::tail -> match List.find ((fun e1 e2 -> e1.attr_name = e2.attr_name) head) tail with
                    | duplicate -> 
                        let () = printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" duplicate.attr_loc class_name duplicate.attr_name in exit 1
                    | exception Not_found -> find_dup_attr class_name tail

let new_class ic =
    let ic_pointer = pos_in ic in
    let input = (input_line ic, input_line ic) in
    let inher = [input_line ic] in
    let inher = if List.hd inher = "inherits" then inher @ [input_line ic; input_line ic] else inher in
    let f_length = input_line ic |> int_of_string in
    let attr_list = new_attr_list ic in
    let () = find_dup_attr (fst input) attr_list in
    let c = {
            name = fst input; 
            loc  = snd input; 
            ic_loc = ic_pointer;
            inherits = inher;
            features_length = f_length;
            attributes = attr_list;
            methods = Hashtbl.create 50 
    } in c;;

let rec new_ast ic length =
    if length = 0 then []
    else let c = new_class ic in 
        c :: new_ast ic (length - 1);;

let _ = retrieve_class_list (open_in Sys.argv.(1)) in 
    let ic = open_in Sys.argv.(1) in   
    let length = input_line ic |> int_of_string in
    let _ = new_ast ic length in 
        close_in ic;
        let oc = String.concat "-ast" [Sys.argv.(1)] in printf "%s\n" oc
(*
Take in AST Tree, spit out class-map, implementation-map, and parent-map.
The tree is composed of:
# of Classes;
Classes -> 
    class name;
    class location;
    class inheritance ->
        yes/no inherits;
        does it inherits? then
        Parent Class;
        Inherits Location;
    feature length;
    Attributes -> 
        ...
    Methods ->
        ...

The idea is to take this and type check it.
To do this, we need to know ahead of time what class to check first because of class inheritance.
We can use Kahn's Alogirthm for this.
Now, we have to type check each feature.
First attributes. Since we are not checking type expressions these are easy.
Second, Methods. Methods ca
*)