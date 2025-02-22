open Printf

(* All type check errors will raise one of these exceptions *)

exception No_class_main
exception No_method_main
exception Cycle of string list
exception Class_redefine of string list
exception Class_unknown of string list
exception Attribute_redefine of string list
exception Attribute_failure of string list
exception Method_formal_length
exception Method_formal_redefine of string list
exception Method_type_redefine of string list
exception Method_failure of string list

(* Data Structures - Used to store the AST while type checking ---------------------------------------------------------------------------------------------- *)
type ast_method = {
    method_name     : string;
    method_formals  : string list list;
    method_type     : string;
    method_type_loc : string;
    method_body     : string list;
}

type ast_attribute = {
    attr_name : string;
    attr_init : string;
    attr_body : string list;
    attr_loc  : string;
    type_loc  : string;
    attr_type : string;
}

type ast_class = {
    name            : string;
    attr_location   : int;
    location        : string;
    inherits        : string list;
    features_length : string;
    attributes      : ast_attribute list;
    methods         : ast_method list
}

let std_classes = [
    {name = "Object"; attr_location = 0; inherits = ["no_inherits"]; location = "0"; features_length = "0"; attributes = []; methods = []};
    {name = "IO";     attr_location = 0; inherits = ["no_inherits"]; location = "0"; features_length = "0"; attributes = []; methods = []};
    {name = "Int";    attr_location = 0; inherits = ["no_inherits"]; location = "0"; features_length = "0"; attributes = []; methods = []};
    {name = "String"; attr_location = 0; inherits = ["no_inherits"]; location = "0"; features_length = "0"; attributes = []; methods = []};
    {name = "Bool";   attr_location = 0; inherits = ["no_inherits"]; location = "0"; features_length = "0"; attributes = []; methods = []}
]

(* Helper Functions - Names are self-explanatory ------------------------------------------------------------------------------------------------------------ *)

(* Temporary Method to skip through attribute initialization - remove later *)
let rec class_skip ic =
    let ic_pointer = pos_in ic in
    try let _ = input_line ic in
    let str = input_line ic in
    if str = String.capitalize_ascii str then
        try let str = input_line ic in 
        seek_in ic ic_pointer;
        if str = "inherits" || str = "no_inherits" then ()
        else let _ = input_line ic in class_skip ic
        with End_of_file -> ();
    else class_skip ic
    with End_of_file -> ();;

let rec create_edges lst = 
    match lst with
    | [] -> []
    | cl::tail -> 
        match List.length cl.inherits with
        | 1 -> ["Object";               cl.name] :: create_edges tail
        | n -> [List.nth cl.inherits 1; cl.name] :: create_edges tail;;

(* Prints a string list in the format: [a1, a2, ..., an]\n
    Used to print the inheritance cycle error             *)
let rec print_cycle cycle =
    match cycle with
    | [] -> ()
    | head::tail -> if tail = [] 
        then printf "%s]\n" head
        else printf "%s, "  head;
        print_cycle tail;;

(*  Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

(*  Checks whether a node has no incoming edge. Useful for calculating what task/class should go to list S (called s in this implementation) *)
let is_source lst node = List.exists (fun ele -> List.nth ele 1 = node) lst |> not

(* Received from a dream - https://stackoverflow.com/questions/30634119/ocaml-removing-duplicates-from-a-list-while-maintaining-order-from-the-right *)
(* Removes all duplicates, keeping the leftmost elements in the list *)
let uniq_cons lst ele = if List.mem ele lst then lst else ele :: lst
let remove_from_left lst = List.rev (List.fold_left uniq_cons [] lst)


(*  Performs Kahn's Algorithm as shown from wikipedia; 
    Returns list of sorted strings or will cry Error if a cycle is found *)
let rec kahn_algorithm s edges = 
    match s with
    | [] -> if List.is_empty edges                                                  (* We check if there are any edges left for cycle special case *)
                then []                                                             (* If edges empty; don't raise error                           *)
                else raise (Cycle (List.map List.hd edges |> remove_from_left));    (* Else, raise error with the cycle in it                      *)
    | head::tail -> 
        let m = List.filter (fun ele -> List.hd ele = head) edges in                (* m is the list of all nodes with one incoming node equal to head *)
            let edges = List.filter (fun edge -> List.mem edge m |> not) edges in   (* We filter m out of list edges                                   *)
                head :: kahn_algorithm (List.fast_sort String.compare ((List.filter (is_source edges) (List.map (fun edge -> List.nth edge 1) m)) @ tail)) edges
                (* ^ This line here adds m to s IF they are sources; THEN sorts new s using string comparison *)
                    
(*  Takes a string list list and returns a topo-sorted string list *)
let topo_sort edges =
    let nodes = List.flatten edges |> remove_from_left in                               (* Effectively a set containing all nodes in edges *)
        let s = List.fast_sort String.compare (List.filter (is_source edges) nodes) in  (* List s stores all sources in nodes              *)
            kahn_algorithm s edges                                                      (* Function call to Kahn's algorithm               *)

(* AST Constructors - Each Method reads a part of the ast tree ---------------------------------------------------------------------------------------------- *)

let rec new_expression ic =
    let (*program_line*) _ = input_line ic in
    match input_line ic with
    | "assign"  -> let (* ident. loc, ident. *)_ = input_line ic, input_line ic in (* Initializer *) new_expression ic
    | "dynamic_dispatch" ->
        let (* ident. *) _ = new_expression ic in let (* method & loc *) _ = input_line ic, input_line ic in 
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ()
        done; []
    | "self_dispatch" ->  
        let (* method & loc *)_ = input_line ic, input_line ic in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ()
        done; []
    | "static_dispatch" -> 
        let (* ident *) _ = new_expression ic in let (* superclass *) _ = input_line ic, input_line ic in 
        let (* method & loc*) _ = input_line ic, input_line ic in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ()
        done; []
    | "block"   -> 
        for i = 1 to input_line ic |> int_of_string (* # of expr. in block *) do
            let _ = new_expression ic in ()
        done; []
    | "identifier" -> let (* ident. loc, ident. *)_ = input_line ic, input_line ic in []
    | "if" -> 
        let (* cond. *) _ = new_expression ic in 
        let (* then  *) _ = new_expression ic in 
        let (* else  *) _ = new_expression ic in [] 
    | "integer" -> let (* integer itself *) _ =  input_line ic in []
    | "isvoid" -> new_expression ic
    | "let" -> 
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do
            let _ = new_expression ic in ()
        done; []
    | "let_binding_init" ->
        let (* ident. loc, ident. *)_ = input_line ic, input_line ic in
        let (* type loc, type *) _ = input_line ic, input_line ic in
            new_expression ic
    | "let_binding_no_init" ->
        let (* ident. loc, ident. *)_ = input_line ic, input_line ic in 
        let (* type loc, type *) _ = input_line ic, input_line ic in []
    | "negate" -> new_expression ic;
    | "new" -> let (* class & loc *) _ = input_line ic, input_line ic in []
    | "not" -> new_expression ic
    | "plus" | "minus" | "times" | "divide" | "eq" | "lt" | "le" -> 
        let _ = (* Left - Side *) new_expression ic, (* Right - Side *) new_expression ic in []
    | "SELF_TYPE" -> []
    | "string" -> let (* string *) _ = input_line ic in []
    | "true" -> []
    | "false" -> []
    | "while" -> let (* cond. *) _ = new_expression ic in (* body *) new_expression ic
    | str -> []

let rec new_formal_list ic length = 
    match length with
    | 0 -> []
    | _ -> [input_line ic; input_line ic; input_line ic; input_line ic] :: new_formal_list ic (length - 1)

let rec find_f lst1 lst2 =
    if List.is_empty lst1 then [""]
    else if List.hd lst1 != List.hd lst2 then List.hd lst2
    else find_f (List.tl lst1) (List.tl lst2)

let rec new_method_list ic env = 
    if input_line ic != "method" then []
    else 
        let loc = input_line ic in
        let name = input_line ic in
        let formals = new_formal_list ic (int_of_string (input_line ic)) in
        let type_loc = input_line ic in
        let m_type = input_line ic in
        let body = new_expression ic in
        let found = Hashtbl.mem env name in
        if not found then
            Hashtbl.add env name ([m_type] :: formals)
        else 
            let prev_f = Hashtbl.find env name in 
            if List.length prev_f != (List.length formals + 1) then
                raise (Method_formal_length)
            else if List.tl prev_f != formals then 
                raise (Method_formal_redefine (find_f (List.tl prev_f) formals)) 
            else if List.hd (List.hd prev_f) != m_type then
                raise (Method_type_redefine ((List.hd prev_f) @ [m_type]))
            else let m = {
                method_name     = name;
                method_formals  = formals;
                method_type     = m_type;
                method_type_loc = type_loc;
                method_body     = []
            } in let return = pos_in ic in
            
    


let rec new_attr_list ic env = 
    let init   = input_line ic in
    if init != "attribute_init" || init != "attribute_no_init" then []
    else 
        let init = if init = "attribute_init" then "initializer" else "no_initaliazer" in
        let loc = input_line ic in
        let name   = input_line ic in
        let t_loc  = input_line ic in
        let a_type = input_line ic in
        let found = Hashtbl.mem env name in 
        if not found then 
            Hashtbl.add env name a_type
        else 
            raise (Attribute_redefine [name; loc]);
        let attr = { 
            attr_name = name;
            attr_init = init;
            attr_loc  = loc;
            type_loc  = t_loc;
            attr_type = a_type; 
            attr_body = new_expression ic;
        } in let return = pos_in ic in
        try match input_line ic with
        | "method" -> seek_in ic return; [attr]
        | line     -> seek_in ic return;  attr :: new_attr_list ic env
        with End_of_file -> [attr];;

let rec new_class_list ic length =
    match length with
    | 0 -> []
    | l ->
        let input = (input_line ic, input_line ic) in
        if List.mem (fst input) ["Bool"; "Object"; "IO"; "Int"; "String"] then
            let () = printf "ERROR: %s: Type-Check: class %s redefined\n" (snd input) (fst input) in exit 1
        else let inheritable = (input_line ic = "inherits") in
        let class_inheritance = if inheritable then "inherits"::[input_line ic; input_line ic] else ["no_inherits"] in
        if inheritable && List.mem (List.nth class_inheritance 1) ["Bool"; "Int"; "String"] then 
            let () = printf "ERROR: %s: Type-Check: class %s inherits from %s\n" (snd input) (fst input) (List.nth class_inheritance 1) in exit 1
        else let feature_length = input_line ic in
        let ic_pointer = pos_in ic in
        if feature_length != "0" then class_skip ic;        (*Permanent for now*)
        let c = {
            name            = fst input; 
            location        = snd input; 
            attr_location   = ic_pointer;
            inherits        = class_inheritance;
            features_length = feature_length;
            attributes      = [];
            methods         = []
        } in c :: new_class_list ic (l - 1);;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let class_type_check acc ele = 
    if List.exists (String.equal ele) acc then raise (Class_Redefine, ele) else ele :: acc

let init_method_hash m_env = 
    Hashtbl.add m_env "abort"     ["0"; "Object"];
    Hashtbl.add m_env "type_name" ["0"; "String"];
    Hashtbl.add m_env "copy"      ["0"; "SELF_TYPE"]; 

let init_io_hash m_env = 
    Hashtbl.add m_env "out_string" ["1"; "String"; "SELF_TYPE"];
    Hashtbl.add m_env "out_int"    ["1": "Int"; "SELF_TYPE"];
    Hashtbl.add m_env "in_string"  ["0"; "String"];
    Hashtbl.add m_env "in_int"     ["0"; "String"];

let rec process_features ic ast path a_env m_env parent = 
    match path with 
    | [] -> []
    | head::tail -> try let cl = List.find (fun e -> e.name = head) ast in
        if cl.features_length = "0" then cl :: process_attributes ic ast tail a_env m_env head
        else let () = seek_in ic cl.attr_location in
            if List.hd cl.inherits != "inherits" || List.nth cl.inherits 1 != parent then
                Hashtbl.clear a_env;
                init_method_hash (Hashtbl.clear m_env);
                if parent = "IO" then init_io_hash m_env;
            try let cl = {cl with attributes = new_attr_list ic a_env} in
            let cl = if input_line ic = "method" 
                then {cl with methods = new_method_list ic m_env}
                else cl
            in cl :: process_attributes ic ast tail a_env m_env head 
            with Attribute_redefine lst -> raise (Attribute_failure (cl.name :: lst))
            with Not_found -> process_attributes ic ast tail a_env m_env head;;

let rec print_attribute oc lst =
    match lst with
    | [] -> ();
    | attr::tail -> 
        fprintf oc "%s\n%s\n" attr.attr_init attr.attr_name;
        List.iter (fprintf oc "%s\n") attr.attr_body;
        print_attribute oc tail;;

let rec print_map oc ast = 
    match ast with
    | [] -> close_out oc
    | cl::tail -> 
        fprintf oc "%s\n%i\n" cl.name (List.length cl.attributes);
        print_attribute oc cl.attributes;
        print_map oc tail;;

(* 'Main' Method of the program. Calls all functions and returns either an error or a .cl-ast file ---------------------------------------------------------- *)
let main ic =
    match input_line ic with                                                                               (* Handles the empty file case *)
    | "0"    -> error No_Main                                                                              (* If no class exists, cry no class Main *)
    | length -> let ast = new_class_list ic (length |> int_of_string) in                                   (* Else read classes superficially *)
    LList.fold_left class_type_check ast
    let path = create_edges ast |> topo_sort in
    let ast = process_features ic ast path (Hashtbl.create 50) (Hashtbl.create 50) "Object" in
        let oc = String.concat "temp" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out in
            fprintf oc "class_map\n%i\n" (List.length ast);
            List.fast_sort (fun cl1 cl2 -> String.compare cl1.name cl2.name) (std_classes @ ast) |> print_map oc; 
        ;;

(* Error handler - Calls main method and handles all errors encountered ------------------------------------------------------------------------------------- *)

let ic = open_in Sys.argv.(1) in (* Opens input file. will crash if file does not exist *)      
    try main ic
with No_class_main ->
    let () = printf "ERROR: 0: Type-Check: class Main not found\n" in exit 1
with No_method_main ->
    let () = printf "ERROR: 0: Type-Check: class Main method main not found\n" in exit 1
with Cycle lst ->                                                           (* Cycle error *)
    printf "ERROR: 0: Type-Check: inheritance cycle: ";                    
    if List.length lst = 1 then                                             (* We want to print only class if cycle contains one element only *)
        let () = printf "%s\n" (List.hd lst) in exit 1                      (* Prints only class name and not brackets: c1                    *)
    else 
        let () = printf "[" in let () = print_cycle lst in exit 1;           (* Prints cycle with brackets and commas: [c1, c2, c3, ..., cn]   *)
with Attribute_failure lst ->
    let () = printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" (List.nth lst 2) (List.hd lst) (List.nth lst 1) in exit 1

    (* Potentially, instead of saving every single line in types, we can just save the type of each attribute, method, and function.
   Then, we can just look them up when we print out the ast tree.
            
    1st Pass: Read classes and type check them. We DONT read attributes nor methods because of lack of info from environment and parents.
        Then we should order classes to know which class to safely type check attributes and methods (not expressions) FIRST.
    2nd Pass: Read the attributes and methods of each class in order to build an environment for attributes and methods for EACH class. We can type check for
        attribute or method redefinition.
    3rd Pass: Now read attributes initializations and method bodies. We have all class info to type check this part.
*)