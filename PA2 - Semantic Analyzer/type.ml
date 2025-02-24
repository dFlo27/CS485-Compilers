open Printf

(* All type check errors will raise one of these exceptions *)

exception No_class_main
exception No_method_main
exception Cycle                      of string list
exception Class_redefine             of string * string
exception Class_inherits             of string list
exception Class_unknown              of string list
exception Attribute_redefine         of string list
exception Attribute_unknown          of string list
exception Method_formal_length       of string list
exception Method_formal_redefine     of string list
exception Method_return_redefine     of string list
exception Method_unknown_formal_type of string list 
exception Method_unknown_return_type of string list

(* Data Structures - The record is to store a class and the rest is for the built-in classes ---------------------------------------------------------------- *)

type a_class = {
    parent          : string;
    attribute_env   : (string, string) Hashtbl.t;
    method_env      : (string, string list) Hashtbl.t;
    ic_pointers     : int list;
}

let empty_a_hash    = Hashtbl.create 0    (* This is for classes with no attributes. Probably will save memory, I think *)
let empty_m_hash    = Hashtbl.create 0    (* This is for classes with no methods.  *)
let m_obj_hash = Hashtbl.create 0         (* This is for Object *)
let () = Hashtbl.add m_obj_hash "abort"     ["Object"   ]
let () = Hashtbl.add m_obj_hash "type_name" ["Object"   ]
let () = Hashtbl.add m_obj_hash "copy"      ["SELF_TYPE"]
let m_io_hash = Hashtbl.create 0          (* This is for IO *)
let () = Hashtbl.add m_io_hash "out_string" ["SELF_TYPE"; "String"]
let () = Hashtbl.add m_io_hash "out_int"    ["SELF_TYPE"; "Int"   ]
let () = Hashtbl.add m_io_hash "in_string"  ["String"]
let () = Hashtbl.add m_io_hash "in_int"     ["Int"   ]
let m_string_hash = Hashtbl.create 0      (* This is for String *)
let () = Hashtbl.add m_string_hash "length" ["Int"]
let () = Hashtbl.add m_string_hash "concat" ["String"; "String"    ]
let () = Hashtbl.add m_string_hash "substr" ["String"; "Int"; "Int"]

let classes    = Hashtbl.create 50        (* Hash containing all classes *)
let () = Hashtbl.add classes "Object" {parent = "";       ic_pointers = []; attribute_env = empty_a_hash; method_env = m_obj_hash   }
let () = Hashtbl.add classes "IO"     {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_io_hash    }
let () = Hashtbl.add classes "Int"    {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = empty_m_hash }
let () = Hashtbl.add classes "String" {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_string_hash}
let () = Hashtbl.add classes "Bool"   {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = empty_m_hash }

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

let create_path class_name class_info path = [class_info.parent; class_name] :: path

let check_parents ic class_name class_info =
    if class_name = "Object" then ()
    else match Hashtbl.mem classes class_info.parent with
    | true  -> ()
    | false -> 
        seek_in ic (List.hd class_info.ic_pointers);                
        raise (Class_unknown [input_line ic; class_name; class_info.parent]) 

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

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)

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
    | 0 -> 
        let type_location = input_line ic in
        let return_type   = input_line ic in 
        if Hashtbl.mem classes return_type |> not then
            raise (Method_unknown_return_type [type_location; return_type]) 
        else []
    | _ -> 
        let (* identifier_location *) _ = input_line ic in
        let (* identifier          *) _ = input_line ic in
        let formal_location = input_line ic in
        let formal_type     = input_line ic in
        if Hashtbl.mem classes formal_type |> not then
            raise (Method_unknown_formal_type [formal_location; formal_type])
        else 
            formal_type :: new_formal_list ic (length - 1)

let rec override_formal ic overridden length =
    if List.is_empty overridden then
        raise (Method_formal_length []);
    match length with
    | 0 -> 
        if List.is_empty (List.tl overridden) then
            raise (Method_formal_length []);
        let type_location = input_line ic in
        let return_type   = input_line ic in 
        if return_type != List.hd overridden then
            raise (Method_return_redefine [type_location; List.hd overridden; return_type]);
    | _ ->
        let (* identifier_location *) _ = input_line ic in
        let identifier      = input_line ic in
        let formal_location = input_line ic in
        let formal_type     = input_line ic in
        if formal_type != List.hd overridden then
            raise (Method_formal_redefine [formal_location; identifier])
        else
            override_formal ic (List.tl overridden) (length - 1)

let rec find_overridden identifier child_name child_info =
    match Hashtbl.find_opt classes child_info.parent with
    | Some parent_info -> 
        let potential_method = Hashtbl.find_opt parent_info.method_env identifier in 
        if potential_method != None then
            Option.get potential_method
        else
            find_overridden identifier child_info.parent parent_info
    | None -> []

let rec a_has_ancestor identifier child_name child_info =
    match child_name with
    | "Object" -> false
    | _ -> 
        if Hashtbl.mem child_info.attribute_env identifier then
            true
        else
            a_has_ancestor identifier child_info.parent (Hashtbl.find classes child_info.parent)

let rec read_features ic class_name class_info length = 
    match length with
    | 0 -> ()
    | _ -> 
        let feature             = input_line ic in
        let identifier_location = input_line ic in
        let identifier          = input_line ic in
        match feature with
        | "method" -> 
            let formals_length  = input_line ic |> int_of_string in
            let formals = find_overridden identifier class_name class_info in
            if List.is_empty formals then
                try Hashtbl.add class_info.method_env identifier (new_formal_list ic formals_length)
                with
                | Method_unknown_formal_type err -> raise (Method_unknown_formal_type [List.hd err; class_name; identifier; List.nth err 1]) 
                | Method_unknown_return_type err -> raise (Method_unknown_return_type [List.hd err; class_name; identifier; List.nth err 1])
            else let () = 
                try override_formal ic formals formals_length
                with 
                | Method_formal_length   err -> raise (Method_formal_length   [identifier_location; class_name; identifier])
                | Method_formal_redefine err -> raise (Method_formal_redefine (err @ [class_name; identifier]))
                | Method_return_redefine err -> raise (Method_return_redefine ([List.hd err; class_name; identifier] @ List.tl err))
                in Hashtbl.add class_info.method_env identifier formals;
            feature_skip ic;
            read_features ic class_name class_info (length - 1)
        | "attribute_init" | "attribute_no_init" ->
            if a_has_ancestor identifier class_name class_info then
                raise (Attribute_redefine [identifier_location; class_name; identifier]);
            let type_location       = input_line ic in
            let type_attribute      = input_line ic in
            if Hashtbl.mem classes type_attribute |> not then
                raise (Attribute_unknown [type_location; class_name; identifier; type_attribute]);
            Hashtbl.add class_info.attribute_env identifier type_attribute;
            if feature = "attribute_init" then
                feature_skip ic;
            read_features ic class_name class_info (length - 1)
        | _ -> raise (Failure "new_features_list failed")

let rec get_features ic path = 
    match path with 
    | [] -> ()
    | head::tail -> 
        let c_class = Hashtbl.find classes head in
        if List.is_empty c_class.ic_pointers then
            get_features ic tail
        else 
            let () = seek_in ic (List.nth c_class.ic_pointers 1)  in
            let feature_length = input_line ic |> int_of_string   in
            let () = read_features ic head c_class feature_length in
                get_features ic tail

let rec read_classes ic length =
    match length with
    | 0 -> ()
    | _ ->
        let class_pointer = pos_in ic in
        let program_line  = input_line ic in
        let class_name    = input_line ic in
        if Hashtbl.mem classes class_name then
            raise (Class_redefine (program_line, class_name))
        else let inheritable = (input_line ic = "inherits") in
        let class_inheritance = if inheritable then (input_line ic, input_line ic) else ("Object", "") in
        if inheritable && List.mem (fst class_inheritance) ["Bool"; "Int"; "String"] then 
            raise (Class_inherits [snd class_inheritance; class_name; fst class_inheritance]);
        let feature_pointer = pos_in   ic in
        class_skip ic (input_line ic |> int_of_string);
        let class_info = {
            parent        = fst class_inheritance;
            attribute_env = Hashtbl.create 50;
            method_env    = Hashtbl.create 50; 
            ic_pointers   = [class_pointer; feature_pointer]
        } in 
        Hashtbl.add classes class_name class_info;
        read_classes ic (length - 1)

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)
let rec print_body ic oc =
    let ic_pointer = pos_in ic in
    try let line       = input_line ic in
    match line with
    | "method" | "attribute_init" | "attribute_no_init" -> seek_in ic ic_pointer; ()
    | _ ->  if int_of_string_opt line = None then 
                let () = fprintf oc "%s\n" line in
                    print_body ic oc;
    with End_of_file -> ();;


let rec print_attributes ic oc length =
    match length with
    | 0 -> ()
    | _ ->
        let inherits = input_line ic in
        if inherits = "attribute_init" then
            fprintf oc "initializer\n"
        else
            fprintf oc "no_initializer\n";
        print_body ic oc;
        print_attributes ic oc (length - 1);;

let rec print_map ic oc path = 
    match path with
    | [] -> close_out oc
    | head::tail -> 
        let cl = Hashtbl.find classes head in
        if List.is_empty cl.ic_pointers |> not then
            let () = seek_in ic (List.nth cl.ic_pointers 1) in
            let length = input_line ic |> int_of_string     in
            let () = fprintf oc "%s\n%i\n" head length      in
            print_attributes ic oc length
        else if head = "IO" then
            fprintf oc "IO\n"
        else
            fprintf oc "%s\n0\n" head;
        print_map ic oc tail;;

(* 'Main' Method of the program. Calls all functions and returns either an error or a .cl-ast file ---------------------------------------------------------- *)
let main ic =
    match input_line ic with                                                                         (* Handles the empty file case                 *)
    | "0"    -> raise No_class_main                                                                  (* If no class exists, cry no class Main       *)
    | length ->                                                                                      
        read_classes ic (length |> int_of_string);                                                   (* Add program classes to hash table classes   *)
        Hashtbl.iter (check_parents ic) classes;                                                     (* Check all parents for any unknown classes   *)
        let path    = List.tl (Hashtbl.fold create_path classes [] |> topo_sort) in                  (* Create a path for feature type checking     *)
        get_features ic path;                                                                        (* List.tl removes Object's parent from path   *)
        let oc = String.concat "temp" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out in
            fprintf oc "class_map\n%i\n" (Hashtbl.length classes);
            List.fast_sort String.compare path |> print_map ic oc;
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
    let () = printf "ERROR: %s: Type-Check: class %s inherits from %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Class_redefine (location, name) ->
    let () = printf "ERROR: %s: Type-Check: class %s redefined\n" location name in exit 1
| Class_unknown  err  ->
    let () = printf "ERROR: %s: Type-Check: class %s inherits unknown class %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Cycle err           ->                                                           (* Cycle error *)
    printf "ERROR: 0: Type-Check: inheritance cycle: ";                    
    if List.length err = 1 then                                             (* We want to print only class if cycle contains one element only *)
        let () = printf "%s\n" (List.hd err) in exit 1                      (* Prints only class name and not brackets: c1                    *)
    else 
        let () = printf "[" in let () = print_cycle err in exit 1           (* Prints cycle with brackets and commas: [c1, c2, c3, ..., cn]   *)
| Attribute_redefine err->
    let () = printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1
| Attribute_unknown err ->
    let () = printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s" 
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) in exit 1;
| Method_formal_length err       ->
    let () = printf "ERROR: %s: Type-Check: class %s redefines method %s and changes number of formals\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) in exit 1;
| Method_formal_redefine err     ->
    let () = printf "ERROR: %s: Type-Check: class %s redefines method %s and changes type of formal %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) in exit 1;  
| Method_return_redefine err    ->
    let () = printf "ERROR: %s: Type-Check: class %s redefines method %s and changes return type (from %s to %s)\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) (List.nth err 4) in exit 1;
| Method_unknown_formal_type err ->
    let () = printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter of unknown type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) in exit 1;
| Method_unknown_return_type err ->
    let () = printf "ERROR: %s: Type-Check: class %s has method %s with unknown return type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) in exit 1;

        (*
    1st Pass: Read classes and type check them. We DONT read attributes nor methods because of lack of info from environment and parents.
        Then we should order classes to know which class to safely type check attributes and methods (not expressions) FIRST.
    2nd Pass: Read the attributes and methods of each class in order to build an environment for attributes and methods for EACH class. We can type check for
        attribute or method redefinition.
    3rd Pass: Now read attributes initializations and method bodies. We have all class info to type check this part.
*)
