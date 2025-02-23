open Printf

(* All type check errors will raise one of these exceptions *)

exception No_class_main
exception No_method_main
exception Cycle                     of string list
exception Class_redefine            of string * string
exception Class_inherits            of string list
exception Class_unknown             of string list
exception Attribute_redefine        of string list
exception Attribute_unknown         of string list
exception Method_formal_length
exception Method_formal_redefine    of string list
exception Method_type_redefine      of string list

(* Data Structures - The record is to store a class and the rest is for the built-in classes ---------------------------------------------------------------- *)

type a_class = {
    name            : string;
    parent          : string;
    attribute_env   : (string, string) Hashtbl.t;
    method_env      : (string, string list) Hashtbl.t;
    ic_pointers     : int list;
}

let class_hash    = Hashtbl.create 50 
let a_std_hash    = Hashtbl.create 0    (* This is for std_classes. Saves memory, I think *)
let m_std_hash    = Hashtbl.create 0    (* This is for Int and Bool, they don't have methods *)
let m_object_hash = Hashtbl.create 0    (* This is for Object *)
let () = Hashtbl.add m_object_hash "abort"     ["Object"]
let () = Hashtbl.add m_object_hash "type_name" ["Object"]
let () = Hashtbl.add m_object_hash "copy"      ["SELF_TYPE"]
let m_io_hash = Hashtbl.create 0        (* This is for IO *)
let () = Hashtbl.add m_io_hash "out_string" ["SELF_TYPE"; "String"]
let () = Hashtbl.add m_io_hash "out_int"    ["SELF_TYPE"; "Int"]
let () = Hashtbl.add m_io_hash "in_string"  ["String"]
let () = Hashtbl.add m_io_hash "in_int"     ["Int"]
let m_string_hash = Hashtbl.create 0    (* This is for String *)
let () = Hashtbl.add m_string_hash "length" ["Int"]
let () = Hashtbl.add m_string_hash "concat" ["String"; "String"]
let () = Hashtbl.add m_string_hash "substr" ["String"; "Int"; "Int"]
let std_classes = [
    {name = "Object"; parent = "";       ic_pointers = []; attribute_env = a_std_hash; method_env = m_object_hash};
    {name = "IO";     parent = "Object"; ic_pointers = []; attribute_env = a_std_hash; method_env = m_io_hash};
    {name = "Int";    parent = "Object"; ic_pointers = []; attribute_env = a_std_hash; method_env = m_std_hash};
    {name = "String"; parent = "Object"; ic_pointers = []; attribute_env = a_std_hash; method_env = m_string_hash};
    {name = "Bool";   parent = "Object"; ic_pointers = []; attribute_env = a_std_hash; method_env = m_std_hash}
]

(* Helper Functions ----------------------------------------------------------------------------------------------------------------------------------------- *)

let rec feature_skip ic =
    let pointer = pos_in ic in
    match input_line ic with
    | "method" | "attribute_no_init" | "attribute_init" -> seek_in ic pointer
    | _ -> feature_skip ic

(* Skips features in class. Moves in_channel to next class *)
let rec class_skip ic feature_length = 
    match feature_length with
    | 0 -> ()
    | _ -> feature_skip ic; class_skip ic (feature_length - 1)

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
    | "assign"  -> let (* ident. loc, ident. *) _ = input_line ic, input_line ic in (* Initializer *) new_expression ic
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
(*
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
*)

let rec parent_check identifier i_location child superclasses =
    if child.parent = "Object" || child.parent = "IO" then 
        false
    else
        let parent = List.find (fun cl -> child.parent = cl.name) superclasses in
        if Hashtbl.mem parent.attribute_env identifier then
            true
        else
            parent_check identifier i_location parent superclasses

let rec new_features_list ic cl class_hash superclasses length = 
    match length with
    | 0 -> ()
    | _ -> 
        let _ = input_line ic in                                                (* feature location *)
        let feature_initial = input_line ic in
        match feature_initial with
        | "method" -> new_features_list ic cl class_hash superclasses (length - 1)
        | "attribute_init" | "attribute_no_init" -> 
            let identifier_location = input_line ic in
            let identifier          = input_line ic in
            if parent_check identifier identifier_location cl superclasses then
                raise (Attribute_redefine [identifier_location; cl.name; identifier]);
            let type_location       = input_line ic in
            let type_attribute      = input_line ic in
            if Hashtbl.mem class_hash type_attribute |> not then
                raise (Attribute_unknown [type_location; cl.name; identifier; type_attribute]);
            Hashtbl.add cl.attribute_env identifier type_attribute;
            if feature_initial = "attribute_init" then

            new_features_list ic cl class_hash superclasses (length - 1)
        | _ -> raise (Failure "new_features_list failed")

let rec get_features acc ic class_hash classes = 
    match classes with 
    | [] -> acc
    | head::tail -> 
        if head.ic_pointers = [] then
            head :: get_features (head :: acc) ic class_hash tail
        else 
            let () = seek_in ic (List.nth head.ic_pointers 1) in
            let feature_length = input_line ic |> int_of_string in
            if feature_length = 0 then 
                head :: get_features (head :: acc) ic class_hash tail
            else
                let () = new_features_list ic head class_hash acc feature_length in
                get_features (head :: acc) ic class_hash tail

let rec new_class_list acc ic length =
    match length with
    | 0 -> acc
    | _ ->
        let class_pointer = pos_in     ic in
        let program_line  = input_line ic in
        let class_name    = input_line ic in
        if List.exists (fun ele -> ele.name = class_name) acc then
            raise (Class_redefine (program_line, class_name))
        else let inheritable = (input_line ic = "inherits") in
        let class_inheritance = if inheritable then (input_line ic, input_line ic) else ("","") in
        if inheritable && List.mem (fst class_inheritance) ["Bool"; "Int"; "String"] then 
            raise (Class_inherits [snd class_inheritance; class_name; fst class_inheritance]);
        let feature_pointer = pos_in   ic in
        class_skip ic (input_line ic |> int_of_string);
        let c = {
            name          = class_name;
            parent        = fst class_inheritance;
            attribute_env = Hashtbl.create 50;
            method_env    = Hashtbl.create 50; 
            ic_pointers   = class_pointer :: feature_pointer :: []
        } in 
        new_class_list (c :: acc) ic (length - 1)

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(*
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
*)
(* 'Main' Method of the program. Calls all functions and returns either an error or a .cl-ast file ---------------------------------------------------------- *)
let main ic =
    match input_line ic with                                                                         (* Handles the empty file case *)
    | "0"    -> raise No_class_main                                                                  (* If no class exists, cry no class Main *)
    | length -> let classes = new_class_list std_classes ic (length |> int_of_string) in             (* Else read classes superficially *)
        List.iter (fun cl -> Hashtbl.add class_hash cl.name true) classes;
        match List.find (fun p -> Hashtbl.mem class_hash p.parent |> not) classes with 
        | unknown -> 
            seek_in ic (List.hd unknown.ic_pointers);
            let _ = input_line ic, input_line ic, input_line ic in
            raise (Class_unknown [input_line ic; unknown.name; unknown.parent])
        | exception Not_found -> 
            List.iter (fun cl -> Hashtbl.add class_hash cl.name true) std_classes;
            let path    = List.map (fun cl -> [cl.parent; cl.name]) classes |> topo_sort in
            let classes = List.map (fun name -> List.find (fun cl -> name = cl.name) classes) path in
            let classes = get_features [] ic class_hash classes in
            let oc = String.concat "temp" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out in
            fprintf oc "class_map\n%i\n" (List.length classes);
            List.fast_sort (fun cl1 cl2 -> String.compare cl1.name cl2.name) classes |> print_map oc;
        ;;

(* Error handler - Calls main method and handles all errors encountered ------------------------------------------------------------------------------------- *)

let ic = open_in Sys.argv.(1) in (* Opens input file. will crash if file does not exist *)      
    try main ic
with
| No_class_main  ->
    let () = printf "ERROR: 0: Type-Check: class Main not found\n" in exit 1 
| No_method_main ->
    let () = printf "ERROR: 0: Type-Check: class Main method main not found\n" in exit 1
| Class_inherits err ->
    let () = printf "ERROR: %s: Type-Check: class %s inherits from %s\n" (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Class_redefine (location, name) ->
    let () = printf "ERROR: %s: Type-Check: class %s redefined\n" location name in exit 1
| Class_unknown  err ->
    let () = printf "ERROR: %s: Type-Check: class %s inherits unknown class %s\n" (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Cycle err ->                                                           (* Cycle error *)
    printf "ERROR: 0: Type-Check: inheritance cycle: ";                    
    if List.length err = 1 then                                             (* We want to print only class if cycle contains one element only *)
        let () = printf "%s\n" (List.hd err) in exit 1                      (* Prints only class name and not brackets: c1                    *)
    else 
        let () = printf "[" in let () = print_cycle err in exit 1           (* Prints cycle with brackets and commas: [c1, c2, c3, ..., cn]   *)
| Attribute_redefine err ->
    let () = printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Attribute_unknown err  ->
    let () = printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s" (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) in exit 1;

    (* Potentially, instead of saving every single line in types, we can just save the type of each attribute, method, and function.
   Then, we can just look them up when we print out the ast tree.
            
    1st Pass: Read classes and type check them. We DONT read attributes nor methods because of lack of info from environment and parents.
        Then we should order classes to know which class to safely type check attributes and methods (not expressions) FIRST.
    2nd Pass: Read the attributes and methods of each class in order to build an environment for attributes and methods for EACH class. We can type check for
        attribute or method redefinition.
    3rd Pass: Now read attributes initializations and method bodies. We have all class info to type check this part.
*)