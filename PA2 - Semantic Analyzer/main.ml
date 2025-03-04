(* Hi 
    This is my type-checker. It's a proper mess. 
    But I'm proud of it. 
*)

open Printf;;
open Hashtbl;;
open Sys;;

(* All type check errors will raise one of these exceptions 
   The program will handle every single error at the end; ie. If you want to see the error message for any error, go to the end.
*)

(* Class Exceptions *)
exception No_class_main;;
exception No_method_main;;
exception Method_main_has_parameters;;
exception Cycle           of string;;
exception Class_redefine  of string * string;;
exception Class_self_type of string;;
exception Class_inherits  of string list;;
exception Class_unknown   of string list;;

(* Feature Exceptions *)
exception Attribute_redefine of string list;;
exception Attribute_self     of string list;;
exception Attribute_unknown  of string list;;
exception Method_redefine            of string list;;
exception Method_formal_duplicate    of string list;;
exception Method_formal_length       of string list;;
exception Method_formal_redefine     of string list;;
exception Method_formal_self         of string list;;
exception Method_return_redefine     of string list;;
exception Method_unknown_formal_type of string list;;
exception Method_unknown_return_type of string list;;

(* Expression Exceptions *)
exception Arithmetic_type of string list;;
exception Assign_self       of string;;
exception Assign_nonconform of string list;;
exception Attribute_nonconform of string list;;
exception Case_self of string;;
exception Dispatch_unknown              of string list;;
exception Dispatch_parameter_mismatch   of int    list;;
exception Dispatch_parameter_nonconform of string list;;
exception Static_dispatch_nonconform    of string list;;
exception Equality_mismatch of string list;;
exception Identifier_unbound of string list;;
exception Let_nonconform of string list;;
exception Let_self       of string;;
exception Method_nonconform of string list;;
exception Negate_type  of string list;;
exception Not_type     of string list;;
exception Unknown_type of string list;;

(* Data Structures - The record is to store a class's info and the hash tables are to store environments and classes ---------------------------------------- *)

(* Type object_id is the basic building block of the other two types. Object_id will represent variables, attributes, formals, and method types
    t_list will contain the types of expressions for attributes with initialization and methods. It will be empty for every other instance of object_id*)
type object_id = {
    a_type : (string * string);
    t_list : string list;
};;

(* Type a_method contains all the important info of a specific instance of a method. 
    All classes will only contain defined methods and not its ancestor/parent methods *)

type a_method = {
    method_name  : string;
    f_parameters : object_id list;
    info         : object_id;
};; 

(* Type a_class, will be referred to a_class in functions, contains all of the important info of a class.
    The Hashtbl classes will contain the pairs (string, a_class), where string is the class name and a_class will be the class info *)
type a_class = {
    parent        : string;
    attribute_env : (string, object_id) Hashtbl.t;
    method_env    : a_method list;
    ic_pointers   : int list;
};;

(* Object_id Functions *)
let new_object_id t = {a_type = (t, t); t_list = []};;          (* Given a type, returns a new object_id *)
let get_static  id = fst id.a_type;;                            (* Returns static type of object_id *)
let get_dynamic id = snd id.a_type;;                            (* Returns dynamic type of object_id *)

(* A_method Functions *)
(* Given a type and pair list containing identifiers and type for each formal, returns a new a_method *)
let new_a_method name t args = {method_name = name; info = new_object_id t; f_parameters = List.map new_object_id args};;
let get_m_static  id = fst id.info.a_type;;                       (* Returns static type of a_method *)
let get_m_dynamic id = snd id.info.a_type;;                       (* Returns dynamic type of a_method *)


(* Base Classes - Stores all the info about COOL's standard classes. Contains empty hashes for later use too ------------------------------------------------ *)

(* empty_a_hash will be used as a placeholder for any class with zero defined attributes. (It may have attributes from its ancestors) *)
let empty_a_hash = Hashtbl.create 0;;     

(* Here is the method initialization of the base classes of COOL *)
let m_obj_list = (new_a_method "abort" "Object"     [])
              :: (new_a_method "copy" "SELF_TYPE"   [])
              :: (new_a_method "type_name" "String" []) :: [];;
let m_io_list = (new_a_method "in_int" "Int"       [])
             :: (new_a_method "in_string" "String" [])
             :: (new_a_method "out_int"   "SELF_TYPE"  ["Int"]   )
             :: (new_a_method "out_string" "SELF_TYPE" ["String"]) :: [];;
let m_str_list = (new_a_method "concat" "String" ["String"])
              :: (new_a_method "length" "Int"    [])
              :: (new_a_method "substr" "String" ["Int"; "Int"]) :: [];;

(* Hashtbl classes will contain all of the classes within the COOL program. Class Object has the special parent "", which will be ignored when
    performing the topological sort later. *)
let classes = Hashtbl.create 50;;
let () = Hashtbl.add classes "Object" {parent = "";       ic_pointers = []; attribute_env = empty_a_hash; method_env = m_obj_list};;
let () = Hashtbl.add classes "IO"     {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_io_list };;
let () = Hashtbl.add classes "Int"    {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = []        };;
let () = Hashtbl.add classes "String" {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_str_list};;
let () = Hashtbl.add classes "Bool"   {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = []        };;

(* Helper Functions ----------------------------------------------------------------------------------------------------------------------------------------- *)

(* Checks if class's parent is defined in the program. If it's not, raise Class_unknown error *)
let has_unknown_parent ic class_name class_info =
    if String.equal class_name "Object" |> not && Hashtbl.mem classes class_info.parent |> not then  
        let () = seek_in ic (List.hd class_info.ic_pointers) in
        let _  = input_line ic, input_line ic, input_line ic  in           
        raise (Class_unknown [input_line ic; class_name; class_info.parent]);;

(* Attribute Functions *)

(* Checks whether an attribute has been defined within one of the class's ancestors. Return boolean of its existence *)
let rec attribute_has_ancestor identifier child_name child_info =
    match child_name with
    | "Object" -> false
    | _        -> 
        if Hashtbl.mem child_info.attribute_env identifier then true
        else attribute_has_ancestor identifier child_info.parent (Hashtbl.find classes child_info.parent);;

(* Returns the # of attributes a given class has. Classes contain only attributes that has been explicitly defined inside the class.
    Every class inheriting from another class will need to recursively search its parents to get all the attributes it should have *)
let rec attribute_length class_name class_info acc =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> acc;
    | _ -> attribute_length class_info.parent (Hashtbl.find classes class_info.parent) (acc + Hashtbl.length class_info.attribute_env);;


(* Method Functions *)
let empty_method = new_a_method "" "" [];;

(* Checks if a method has been already declared before. Checks parents as well. Returns the method or the empty_method defined above *)
let rec find_method identifier a_class =
    try List.find (fun e -> String.equal e.method_name identifier) a_class.method_env
    with Not_found -> 
        if String.equal a_class.parent "" |> not then 
            find_method identifier (Hashtbl.find classes a_class.parent)
        else empty_method;;

(* Returns a list that contains all of the methods a class defined and inherits. Magical isn't it? *)
let rec get_method_list class_name a_class =
    let class_methods = List.fold_right (fun e acc -> (class_name, e.method_name) :: acc) a_class.method_env [] in
    if String.equal class_name "Object" |> not then
        List.filter (fun e1 -> List.exists (fun e2 -> String.equal (snd e1) (snd e2)) class_methods |> not) 
            (get_method_list a_class.parent (Hashtbl.find classes a_class.parent)) @ class_methods
    else class_methods;;

(* Least Upper Bound Function - Computes the common ancestor class of two classes. Will always return a class hopefully. ------------------------------------ *)

(* This function will compare two class's ancestor branch (which is a list) and return the last common ancestor *)
let rec find_common_ancestor tree_1 tree_2 prev_class = 
    if tree_1 = [] || tree_2 = [] || (String.equal (List.hd tree_1) (List.hd tree_2) |> not) then
        prev_class
    else find_common_ancestor (List.tl tree_1) (List.tl tree_2) (List.hd tree_1);;

(* This function will create a list containing all of the class's parents starting from the class and finishing at class Object *)
let rec get_ancestor_tree a_class =
    if String.equal a_class "Object" |> not then 
        let c_info = Hashtbl.find classes a_class in
        a_class :: get_ancestor_tree c_info.parent
    else ["Object"]

(* Least Upper Bound Function : Returns the closest ancestor of two classes *)
let lub class_1 class_2 = 
    if String.equal class_1 class_2 then class_1
    else 
        let class_1_ancestors = get_ancestor_tree class_1 |> List.rev in
        let class_2_ancestors = get_ancestor_tree class_2 |> List.rev in
        find_common_ancestor class_1_ancestors class_2_ancestors "Object";;

(* Methods for Skipping parts of the AST -------------------------------------------------------------------------------------------------------------------- *)

(* Provides a way to skip expressions
    There are three cases:
        1. If a feature is found (input = method/attribute_...) then set ic back to 'start' (ic_pointer_1) and return
        2. If a feature isn't found but the 3rd line scanned is inherits/no_inherits, then move ic back to 'start' (ic_pointer_1) and return
            ^ Classes are stored in the AST as: class_location, class_name, inherits, ...; so its important to go back to 'start' to keep information
        3. If nothing is found then move ic by one line (go to ic_pointer_2) and call itself
*)
let rec expressions_skip ic =
    let ic_pointer_1 = pos_in ic in
    try let input = input_line ic in
    if String.equal input "method" || String.equal input "attribute_init" || String.equal input "attribute_no_init" then
        seek_in ic ic_pointer_1
    else 
        let ic_pointer_2 = pos_in ic in
        try let _     = input_line ic in
        try let input = input_line ic in
        if  String.equal input "inherits" || String.equal input "no_inherits" then
            seek_in ic ic_pointer_1
        else let () = seek_in ic ic_pointer_2 in
            expressions_skip ic;
    with End_of_file -> () with End_of_file -> () with End_of_file -> ();;               (* This'll stop any exceptions from annoying me *)

(* When skipping features is necessary. Calls expressions_skip according to the number of features in the class being read *)
let rec features_skip ic length =
    match length with 
    | 0 -> ();
    | _  -> let _ = input_line ic in
            expressions_skip ic; 
            features_skip ic (length - 1);;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* Checks whether a class (c_type) is a child of another class (p_type). Returns a boolean of the c_type's legitamacy of lineage *)
let rec conforms_to p_type c_type = 
    if String.equal c_type "Object" |> not && String.equal c_type p_type |> not then
        conforms_to p_type (Hashtbl.find classes c_type).parent
    else String.equal c_type p_type 

(*  *)
let handle_main class_info = 
    let m = find_method "main" class_info in
    if m = empty_method then
        raise (No_method_main);
    if m.f_parameters != [] then
        raise (Method_main_has_parameters);;

let rec print_cycle start curr =
    if String.equal start curr |> not then
        let () = printf " %s" curr in
        print_cycle start (Hashtbl.find classes curr).parent;;


(*  Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

(* Adds a parent to child edge to a list that will be sorted by topological sort *)
let create_edge class_name class_info edges = [class_info.parent; class_name] :: edges;;

(*  Checks whether a node has no incoming edge. Useful for calculating what task/class should go to list S (called s in this implementation) *)
let is_source lst node = List.exists (fun ele -> List.nth ele 1 = node) lst |> not;;

(*  Performs Kahn's Algorithm as shown from wikipedia; 
    Returns list of sorted strings or will cry Error if a cycle is found *)
let rec kahn_algorithm s edges = 
    match s with
    | [] -> if List.compare_length_with edges 0 = 0                                 (* We check if there are any edges left for cycle special case *)
                then []                                                             (* If edges empty; don't raise error                           *)
                else raise (Cycle (List.hd edges |> List.hd));                      (* Else, raise error with the cycle in it                      *)
    | head::tail -> 
        let m = List.filter (fun ele -> List.hd ele = head) edges in                (* m is the list of all nodes with one incoming node equal to head *)
            let edges = List.filter (fun edge -> List.mem edge m |> not) edges in   (* We filter m out of list edges                                   *)
                head :: kahn_algorithm (List.fast_sort String.compare ((List.filter (is_source edges) (List.map (fun edge -> List.nth edge 1) m)) @ tail)) edges;;
                (* ^ This line here adds m to s IF they are sources; THEN sorts new s using string comparison *)

(*  Takes a string list list and returns a topo-sorted string list *)
let topo_sort classes =
    let edges = Hashtbl.fold create_edge classes [] in
    let nodes = List.flatten edges |> List.sort_uniq String.compare in                  (* Effectively a set containing all nodes in edges *)
    let s = List.fast_sort String.compare (List.filter (is_source edges) nodes) in      (* List s stores all sources in nodes              *)
        kahn_algorithm s edges |> List.tl;;                                             (* Function call to Kahn's algorithm               *)

(* AST Tree Parser Functions -------------------------------------------------------------------------------------------------------------------------------- *)

let rec read_expression ic class_info =                                                 (* Reads expressions for now *)
    let program_line = input_line ic in                                         
    match input_line ic with
    | "assign" -> 
        let (* ident. loc *) _ = input_line ic in 
        let identifier = input_line ic in
        if String.equal identifier "self" then
            raise (Assign_self program_line);
        let ident_info = Hashtbl.find_opt class_info.attribute_env identifier in
        if ident_info = None then 
            raise (Unknown_type [program_line; identifier]);
        let ident_info = Option.get ident_info in let init_type = read_expression ic class_info in
        if conforms_to (get_static ident_info) (List.hd init_type) |> not then
            raise (Assign_nonconform [program_line; List.hd init_type; get_static ident_info]);
        Hashtbl.add class_info.attribute_env identifier {a_type = (get_static ident_info, List.hd init_type); t_list = []};
        List.hd init_type :: init_type
    | "block" -> 
        let block_type = ref "" in 
        let type_list = block_type_check ic class_info block_type (input_line ic |> int_of_string) in
        !block_type :: type_list
    | "case"  ->
        let case_type = ref "" in
        let case_list = (read_expression ic class_info) @ case_type_check ic class_info case_type (input_line ic |> int_of_string (* # of arguments *)) in
        !case_type :: case_list;
    | "dynamic_dispatch" ->
        let ident_list = read_expression ic class_info in 
        let method_loc = input_line ic in let method_name  = input_line ic in 
        let a_method = find_method method_name (Hashtbl.find classes (List.hd ident_list)) in 
        if a_method = empty_method then
            raise (Dispatch_unknown [method_loc; method_name; List.hd ident_list]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        get_m_static a_method :: ident_list @ (dispatch_type_check ic class_info a_method.f_parameters program_line 0 (input_line ic |> int_of_string));
    | "self_dispatch" ->  
        let method_loc = input_line ic in let method_name = input_line ic in 
        let a_method = find_method method_name class_info in
        if a_method = empty_method then 
            raise (Dispatch_unknown [method_loc; method_name; Hashtbl.fold (fun k e acc -> if e = class_info then k else acc) classes ""]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        get_m_static a_method :: (dispatch_type_check ic class_info a_method.f_parameters program_line 0 (input_line ic |> int_of_string));
    | "static_dispatch" -> 
        let ident_list = read_expression ic class_info in 
        let (* parent_loc *) _ = input_line ic in let parent = input_line ic in 
        if conforms_to parent (List.hd ident_list) |> not then
            raise (Static_dispatch_nonconform [program_line; List.hd ident_list; parent]);
        let method_loc = input_line ic in let method_name  = input_line ic in 
        let a_method = find_method method_name (Hashtbl.find classes (List.hd ident_list)) in 
        if a_method = empty_method then
            raise (Dispatch_unknown [method_loc; method_name; parent]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        get_m_static a_method :: ident_list @ (dispatch_type_check ic class_info a_method.f_parameters program_line 0 (input_line ic |> int_of_string));
    | "eq" | "lt" | "le" -> 
        let left_type = read_expression ic class_info in let right_type = read_expression ic class_info in
        if String.equal (List.hd left_type) (List.hd right_type) |> not then
            raise (Equality_mismatch [program_line; List.hd left_type; List.hd right_type]);
        "Bool" :: left_type @ right_type;
    | "identifier" -> 
        let (* ident_loc *) _ = input_line ic in let identifier = input_line ic in 
        if String.equal identifier "self" |> not then
            let object_id = Hashtbl.find_opt class_info.attribute_env identifier in
            if  object_id = None then
                raise (Identifier_unbound [program_line; identifier])
            else [Option.get object_id |> get_static];
        else ["SELF_TYPE"];
    | "if" -> 
        let cond_type = read_expression ic class_info in 
        let then_type = read_expression ic class_info in 
        let else_type = read_expression ic class_info in 
        lub (List.hd then_type) (List.hd else_type) :: (cond_type @ then_type @ else_type); 
    | "integer" -> let (* integer *) _ =  input_line ic in ["Int"];
    | "isvoid"  -> "Bool" :: read_expression ic class_info;
    | "let" ->  
        let type_list = let_binding_type_check ic class_info program_line  (input_line ic |> int_of_string (* # of bindings *)) in
        let let_type = read_expression ic class_info in
        List.fold_left (fun acc ele -> Hashtbl.remove class_info.attribute_env (fst ele); acc @ (snd ele)) [] type_list @ let_type;
    | "negate" -> 
        let negate_list = read_expression ic class_info in
        if String.equal (List.hd negate_list) "Bool" |> not then
            raise (Negate_type [program_line; List.hd negate_list]);
        "Int" :: negate_list;
    | "new" -> 
        let class_loc = input_line ic in let new_class = input_line ic in 
        if Hashtbl.mem classes new_class |> not then 
            raise (Unknown_type [class_loc; new_class]);
        [new_class];         
    | "not" -> 
        let not_list = read_expression ic class_info in
        if String.equal (List.hd not_list) "Bool" |> not then 
            raise (Not_type [program_line; List.hd not_list]);
        "Bool" :: not_list;
    | "plus" | "minus" | "times" | "divide" -> 
        let left_type = read_expression ic class_info in let right_type = read_expression ic class_info in 
        if String.equal (List.hd left_type) "Int" && String.equal (List.hd right_type) "Int" then 
            "Int" :: (left_type @ right_type)
        else raise (Arithmetic_type [program_line; List.hd left_type; List.hd right_type]);
    | "string" -> let (* string *) _ = input_line ic in ["String"];
    | "true" | "false" -> ["Bool"];
    | "while"  -> 
        let cond_type = read_expression ic class_info in 
        let body_type = read_expression ic class_info in "Object" :: cond_type @ body_type;
    | str -> raise (Failure "read_expression failed") 
and block_type_check ic class_info block_type length =
    match length with
    | 0 -> block_type := "Object"; [];
    | 1 -> 
        let type_list = read_expression ic class_info in
        block_type := List.hd type_list;
        type_list;
    | _ -> read_expression ic class_info @ block_type_check ic class_info block_type (length - 1); 
and case_type_check ic class_info case_type length = 
    match length with
    | 0 -> []
    | _ ->
        let ident_loc = input_line ic in let identifier = input_line ic in
        if String.equal identifier "self" then
            raise (Case_self ident_loc);
        let (* type loc *)   _ = input_line ic in let ident_type = input_line ic in
        if Hashtbl.mem classes ident_type |> not then
            raise (Unknown_type [ident_loc; ident_type]);
        let case_body = read_expression ic class_info in 
        if String.equal !case_type "" then
            case_type := List.hd case_body
        else
            case_type := lub !case_type (List.hd case_body);
        case_body @ case_type_check ic class_info case_type (length - 1)
and dispatch_type_check ic class_info formal_types program_line step length = 
    if step != length then
        let arg_type = read_expression ic class_info in
        if conforms_to (List.hd formal_types |> get_static) (List.hd arg_type) |> not then
            raise (Dispatch_parameter_nonconform [program_line; step + 1 |> string_of_int; List.hd formal_types |> get_static])
        else arg_type @ dispatch_type_check ic class_info (List.tl formal_types) program_line (step + 1) length;
    else [];
and let_binding_type_check ic class_info program_line length = 
    if length = 0 then []
    else 
        let binding = input_line ic in 
        let ident_loc  = input_line ic in let identifier = input_line ic in
        let (* type_loc  *) _ = input_line ic in let bind_type  = input_line ic in
        if String.equal identifier "self" then
            raise (Let_self ident_loc);
        if String.equal identifier "SELF_TYPE" |> not && Hashtbl.mem classes bind_type |> not then
            raise (Unknown_type [program_line; bind_type]);
        Hashtbl.add class_info.attribute_env identifier (new_object_id bind_type);
        if String.equal binding "let_binding_init" then
            let init_type = read_expression ic class_info in
            if conforms_to (List.hd init_type) bind_type |> not then
                raise (Let_nonconform [program_line; List.hd init_type; bind_type])
            else (identifier, init_type) :: let_binding_type_check ic class_info program_line (length - 1);
        else (identifier, []) :: let_binding_type_check ic class_info program_line (length - 1);;

let rec initialize_formals ic class_info formal_types length = 
    match length with 
    | 0 -> []
    | _ ->
        let (* identifier_location *) _ = input_line ic in let identifier  = input_line ic in
        let formal_type = List.hd formal_types |> get_static in 
        Hashtbl.add class_info.attribute_env identifier (new_object_id formal_type);
        identifier :: initialize_formals ic class_info formal_types (length - 1);;

let rec type_check_features ic class_info feature_length =
    if feature_length != 0 then
        let feature    = input_line ic in
        let ident_loc = input_line ic in let identifier = input_line ic in
        match feature with
        | "attribute_no_init" -> 
            expressions_skip ic;
            type_check_features ic class_info (feature_length - 1);
        | "attribute_init"    ->
            let attribute = Hashtbl.find class_info.attribute_env identifier in
            let type_list = read_expression ic class_info in
            if conforms_to (get_static attribute) (List.hd type_list) then
                raise (Attribute_nonconform [ident_loc; List.hd type_list; get_static attribute]);
            Hashtbl.replace class_info.attribute_env identifier {a_type = (get_static attribute, List.hd type_list); t_list = type_list};
            type_check_features ic class_info (feature_length - 1)
        | "method" ->
            let a_method = find_method identifier class_info in
            let formal_list = initialize_formals ic class_info a_method.f_parameters (input_line ic |> int_of_string) in 
            let _  = input_line ic, input_line ic in (* loc and method_type*)
            let type_list = read_expression ic class_info in
            if conforms_to (get_m_static a_method) (List.hd type_list) |> not then
                raise (Method_nonconform [ident_loc; List.hd type_list; get_m_static a_method; identifier]);
            List.iter (fun e -> Hashtbl.remove class_info.attribute_env e) formal_list;
            {a_method with info = {a_type = (get_m_static a_method, List.hd type_list); t_list = type_list}} :: type_check_features ic class_info (feature_length - 1);
        | _ -> raise (Failure "initialize_features failed");
    else [];;

let rec compare_formals_body ident_and_loc prev_formals new_formals =
    match ident_and_loc with
    | [("", r_loc)] -> 
        if String.equal (fst prev_formals) (fst new_formals) |> not then
            raise (Method_return_redefine [r_loc; fst prev_formals; fst new_formals]);
    | (i_loc, ident) :: tail ->
        let prev_formal = (snd prev_formals) |> List.hd in let new_formal = (snd new_formals) |> List.hd in
        if String.equal (get_static prev_formal) (get_static new_formal) |> not then 
            raise (Method_formal_redefine [i_loc; ident])
        else compare_formals_body tail (fst prev_formals, snd prev_formals |> List.tl) (fst new_formals, snd new_formals |> List.tl)
    | [] -> raise (Failure "compare_formals_body is bad")
let compare_formals loc_and_ident prev_method new_method =
    compare_formals_body loc_and_ident (get_m_static prev_method, prev_method.f_parameters) (get_m_static new_method, prev_method.f_parameters);;

let rec new_formals_body ic method_name formal_list extra_info length = 
    match length with
    | 0 -> 
        let type_location = input_line ic in let return_type = input_line ic in 
        if String.equal return_type "SELF_TYPE" |> not && Hashtbl.mem classes return_type |> not then
            raise (Method_unknown_return_type [type_location; return_type]);
        new_a_method method_name return_type (List.rev formal_list), (("", type_location) :: extra_info |> List.rev);
    | _ -> 
        let identifier_location = input_line ic in let identifier  = input_line ic in
        let type_location       = input_line ic in let formal_type = input_line ic in
        if String.equal identifier "self" 
            then raise (Method_formal_self [identifier_location]);
        if List.exists (fun ele -> String.equal (fst ele) identifier) extra_info 
            then raise (Method_formal_duplicate [identifier_location; identifier]);
        if Hashtbl.mem  classes formal_type |> not then 
            raise (Method_unknown_formal_type [type_location; formal_type])
        else new_formals_body ic method_name (formal_type :: formal_list) ((identifier, type_location) :: extra_info) (length - 1);;
let new_formals ic method_name length = new_formals_body ic method_name [] [] length;;

let rec read_features acc ic class_name class_info length = 
    match length with
    | 0 -> acc |> List.rev;
    | _ -> 
        let feature        = input_line ic in
        let ident_location = input_line ic in
        let identifier     = input_line ic in
        match feature with
        | "attribute_init" | "attribute_no_init" ->
            if String.equal identifier "self" then 
                raise (Attribute_self [ident_location; class_name]);
            if attribute_has_ancestor identifier class_name class_info then
                raise (Attribute_redefine [ident_location; class_name; identifier]);
            let type_location  = input_line ic in
            let type_attribute = input_line ic in
            if String.equal type_attribute "SELF_TYPE" |> not && Hashtbl.mem classes type_attribute |> not then
                raise (Attribute_unknown [type_location; class_name; identifier; type_attribute]);
            Hashtbl.add class_info.attribute_env identifier (new_object_id type_attribute);
            let () = if feature = "attribute_init" then
                let _ = expressions_skip ic in ();
            in read_features acc ic class_name class_info (length - 1);
        | "method" -> 
            if List.exists (fun e -> String.equal e.method_name identifier) class_info.method_env then
                raise (Method_redefine [ident_location; class_name; identifier]);
            let formals_length  = input_line ic |> int_of_string in
            let prev_method = find_method identifier (Hashtbl.find classes class_info.parent) in
            let method_info = try new_formals ic identifier formals_length with
            | Method_formal_duplicate    err -> raise (Method_formal_duplicate    [List.hd err; class_name; List.nth err 1]            );
            | Method_formal_self         err -> raise (Method_formal_self         [List.hd err; class_name; identifier    ]            );
            | Method_unknown_formal_type err -> raise (Method_unknown_formal_type [List.hd err; class_name; identifier; List.nth err 1]); 
            | Method_unknown_return_type err -> raise (Method_unknown_return_type [List.hd err; class_name; identifier; List.nth err 1]); 
            in let () = if prev_method != empty_method then
                if prev_method.f_parameters |> List.length != formals_length then
                    raise (Method_formal_length [ident_location; class_name; identifier])
                else try compare_formals (snd method_info) prev_method (fst method_info) with 
                | Method_formal_redefine err -> raise (Method_formal_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
                | Method_return_redefine err -> raise (Method_return_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
            in let () = expressions_skip ic in
            read_features ((fst method_info) :: acc) ic class_name class_info (length - 1);
        | _ -> raise (Failure "read_features failed");;

let add_expressions ic class_name class_info =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> ()
    | _ ->
        seek_in ic (List.nth class_info.ic_pointers 1);
        let feature_length = input_line ic |> int_of_string in
        Hashtbl.replace classes class_name {class_info with method_env = type_check_features ic class_info feature_length};;

let rec add_features ic path =
    match path with
    | [] -> ();
    | head::tail ->
        let class_info = Hashtbl.find classes head in
        (* seek_in is in if expression so that I can avoid using a let statement in the then expression (don't touch) *)
        if List.compare_length_with class_info.ic_pointers 0 != 0 && () = seek_in ic (List.nth class_info.ic_pointers 1) then
            Hashtbl.replace classes head {class_info with method_env = read_features [] ic head class_info (input_line ic |> int_of_string)};
        if String.equal head "Main" then 
            handle_main (Hashtbl.find classes head);
        add_features ic tail;;
    
let rec add_classes ic length =
    match length with
    | 0 -> ();
    | _ ->
        let class_pointer = pos_in ic     in
        let program_line  = input_line ic in
        let class_name    = input_line ic in
        if class_name = "SELF_TYPE" then
            raise (Class_self_type program_line);
        if Hashtbl.mem classes class_name then
            raise (Class_redefine (program_line, class_name)); 
        let inheritable = (String.equal (input_line ic) "inherits") in
        let class_inheritance = if inheritable then (input_line ic, input_line ic) else ("Object", "") in
        if inheritable && List.mem (fst class_inheritance) ["Bool"; "Int"; "String"] then 
            raise (Class_inherits [snd class_inheritance; class_name; fst class_inheritance]);
        let feature_pointer = pos_in ic in
        let feature_length = input_line ic |> int_of_string in 
        features_skip ic feature_length;
        let class_info = {
            parent        = fst class_inheritance;
            attribute_env = Hashtbl.create 50;
            method_env    = [];
            ic_pointers   = [class_pointer; feature_pointer];
        } in Hashtbl.add classes class_name class_info;
        add_classes ic (length - 1);;

(* Print to .cl-ast Functions - There are four main components: class_map, implementation_map, parent_map, and the annotated AST tree ``````----------------------------------------------- *)

let rec print_expressions ic oc type_list =
    fprintf oc "%s\n" (input_line ic);                                          (* Program line of expression *)
    let keyword = input_line ic in
    fprintf oc "%s\n%s\n" (List.hd type_list) keyword;
    let type_list = List.tl type_list in
    match keyword with
    | "assign" -> 
        fprintf oc "%s\n" (input_line ic);                  (* Program line & Ident *)
        fprintf oc "%s\n" (input_line ic);
        print_expressions ic oc type_list;
    | "block" -> 
        let block_length = input_line ic |> int_of_string in
        fprintf oc "%i\n" block_length;
        print_block ic oc type_list block_length;
    | "case"  ->
        let type_list = print_expressions ic oc type_list in
        let case_length = input_line ic |> int_of_string in
        fprintf oc "%i\n" case_length;
        print_case ic oc type_list case_length;
    | "dynamic_dispatch" ->
        let type_list = print_expressions ic oc type_list in 
        fprintf oc "%s\n" (input_line ic);                                  (* Location and Method *)  
        fprintf oc "%s\n" (input_line ic);
        print_dispatch ic oc type_list (input_line ic |> int_of_string);
    | "self_dispatch" ->  
        fprintf oc "%s\n" (input_line ic);                                      (* Location and Method *)
        fprintf oc "%s\n" (input_line ic);
        print_dispatch ic oc type_list (input_line ic |> int_of_string)
    | "static_dispatch" -> 
        let type_list = print_expressions ic oc type_list in 
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);   (* Location and Parent *)
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
        print_dispatch ic oc type_list (input_line ic |> int_of_string);
    | "eq" | "lt" | "le" | "plus" | "minus" | "times" | "divide" | "while" -> 
        print_expressions ic oc type_list |>  print_expressions ic oc;
    | "identifier" -> 
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic);                                  (* Location and identifier *) 
        type_list;
    | "if" -> 
        print_expressions ic oc type_list |> print_expressions ic oc |> print_expressions ic oc (* if expr -> then expr -> else expr *)
    | "integer" | "string" -> 
        fprintf oc "%s\n" (input_line ic);
        type_list;
    | "isvoid" | "negate" | "not" -> 
        print_expressions ic oc type_list;
    | "let" ->  
        print_let_binding ic oc type_list (input_line ic |> int_of_string (* # of bindings *)) |> print_expressions ic oc;
    | "new" -> 
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic);
        type_list;
    | "true" | "false" -> type_list;
    | str -> raise (Failure "print_expression failed");
and print_block ic oc type_list length =
    match length with 
    | 0 -> type_list
    | _ -> 
        let type_list = print_expressions ic oc type_list in
        print_block ic oc type_list (length - 1)
and print_case ic oc type_list length = 
    match length with
    | 0 -> type_list
    | _ ->
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);  (* Program lines, Ident, and type *)
        let type_list = print_expressions ic oc type_list in                                            (* Case Body *)
        print_case ic oc type_list (length - 1);
and print_dispatch ic oc type_list length = 
    match length with
    | 0 -> type_list;
    | _ -> 
        let type_list = print_expressions ic oc type_list in                                            (* argument type list *)
        print_dispatch ic oc type_list (length - 1)
and print_let_binding ic oc type_list length = 
    if length = 0 then []
    else 
        let binding = input_line ic in 
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);  (* Locations, Identifier, and type *)
        if String.equal binding "let_binding_init" then
            print_let_binding ic oc (print_expressions ic oc type_list) (length - 1)
        else print_let_binding ic oc type_list (length - 1);;

let rec print_argument ic oc type_list length = 
    match length with 
    | 0 -> ()
    | _ ->
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic); (* location and identifier  *)
        print_argument ic oc (print_expressions ic oc type_list) (length - 1);;

let rec print_features ic oc class_info = 
    let feature = input_line ic in
    fprintf oc "%s\n%s\n" feature (input_line ic); (* Program line of identifier *)
    let identifier = input_line ic in
    fprintf oc "%s\n" identifier;
    match feature with
    | "attribute_init" | "attribute_no_init" -> 
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic); (* Program line of type and the type of attribute *)
        if String.equal feature "attribute_init" then
            let _ = print_expressions ic oc (Hashtbl.find class_info.attribute_env identifier).t_list in ();
    | "method" -> 
        let a_method =  List.find (fun e -> String.equal e.method_name identifier) class_info.method_env in
        fprintf oc "%s\n" (input_line ic);
        print_argument ic oc a_method.info.t_list (List.length a_method.f_parameters); 
        fprintf oc "%s\n" (input_line ic);
        fprintf oc "%s\n" (input_line ic);
        let _ = print_expressions ic oc a_method.info.t_list in ();
    | _ -> raise (Failure "Print_features read something that wasn't a feature");;

let rec print_annotated_ast ic oc length = 
    match length with
    | 0 -> ()
    | _ -> 
        fprintf oc "%s\n" (input_line ic);                          (* Program line of class definition *)
        let a_class = input_line ic in 
        fprintf oc "%s\n" a_class;
        let inheritable = input_line ic in 
        fprintf oc "%s\n" inheritable;
        if String.equal inheritable "inherits" then 
            let () = fprintf oc "%s\n" (input_line ic) in fprintf oc "%s\n" (input_line ic);
        fprintf oc "%s\n" (input_line ic);
        print_features ic oc (Hashtbl.find classes a_class);
        print_annotated_ast ic oc (length - 1);;

let rec print_parent oc path = 
    match path with
    | [] -> ();
    | head::tail -> 
        if String.equal head "Object" |> not then 
            fprintf oc "%s\n%s\n" head (Hashtbl.find classes head).parent;
        print_parent oc tail;;

let rec print_methods ic oc method_list = 
    match method_list with
    | [] -> ()
    | head::tail -> 
        let class_name  = fst head in
        let method_name = snd head in
        let a_class  = Hashtbl.find classes (class_name) in
        let a_method = List.find (fun e -> String.equal e.method_name method_name) a_class.method_env in
        let () = fprintf oc "%s\n" method_name in 
        let () = if List.mem class_name ["Bool"; "IO"; "Int"; "Object"; "String"] then
            let () = fprintf oc "%i\n" (List.length a_method.f_parameters)  in
            let () = match method_name with                                     (* Too late now to store the formal names *)
            | "concat" -> fprintf oc "s\n"
            | "out_int"    -> fprintf oc "x\n"
            | "out_string" -> fprintf oc "x\n"
            | "substr" -> fprintf oc "i\nl\n" 
            | str -> ()
            in fprintf oc "%s\n0\n%s\ninternal\n%s.%s\n" class_name (get_m_static a_method) class_name method_name
        else
            let _ = input_line ic, input_line ic in                             (* skips 'method' and method name location *)
            let () = while not (String.equal (input_line ic) method_name) do    (* skips overriden methods *)
                expressions_skip ic;
                let _ = input_line ic, input_line ic in ();
            done in
            let () = fprintf oc "%i\n" (input_line ic |> int_of_string) in      (* Prints # of parameters *)
            let () = for i = 1 to List.length a_method.f_parameters do          (* Prints formal parameter identifier *)
                let _ = input_line ic in                                        (* Formal location *)
                fprintf oc "%s\n" (input_line ic);
                let _ = input_line ic, input_line ic in ();                     (* Formal type and location *)
            done in
            let _ = input_line ic, input_line ic in                             (* skips method return type and location *)
            let () = fprintf oc "%s\n" class_name; in
            let _  = print_expressions ic oc a_method.info.t_list in ();
        in print_methods ic oc tail;;

let rec print_implementation ic oc path =
    match path with
    | [] -> ()
    | head::tail -> 
        fprintf oc "%s\n" head;
        let class_info = Hashtbl.find classes head in
        let method_list = get_method_list head class_info in
        fprintf oc "%i\n" (List.length method_list);
        (* Doesn't this look familiar? Oh yeah! its that weird seek_in comparison. I don't like let with ifs *)
        if List.mem head ["Bool"; "IO"; "Int"; "Object"; "String"] |> not && () = seek_in ic (List.nth class_info.ic_pointers 1) then
            expressions_skip ic;
        print_methods ic oc method_list;
        print_implementation ic oc tail;;

let rec print_class_attributes ic oc attribute_env length =
    match length with
    | 0 -> ();
    | _ -> 
        let inherits  = input_line ic in 
        let (* ident. loc.*) _ = input_line ic in
        let identifier         = input_line ic in
        let (* type loc.  *) _ = input_line ic in
        let attribute_type     = input_line ic in
        if String.equal inherits "attribute_init" then 
            fprintf oc "initializer\n"
        else fprintf oc "no_initializer\n";
        fprintf oc "%s\n%s\n" identifier attribute_type;
        if String.equal inherits "attribute_init" then
            let _ = print_expressions ic oc (Hashtbl.find attribute_env identifier).t_list in ();
        print_class_attributes ic oc attribute_env (length - 1);;

let rec print_all_attributes ic oc class_name class_info =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> ();
    | _ ->
        print_all_attributes ic oc class_info.parent (Hashtbl.find classes class_info.parent);
        seek_in ic (List.nth class_info.ic_pointers 1);
        let (* feature length *) _ = input_line ic in
        print_class_attributes ic oc class_info.attribute_env (Hashtbl.length class_info.attribute_env);;

let rec print_class ic oc path = 
    match path with
    | [] -> ()
    | head::tail -> 
        fprintf oc "%s\n" head;
        let class_info = Hashtbl.find classes head in
        fprintf oc "%i\n" (attribute_length head class_info 0);
        if List.compare_length_with class_info.ic_pointers 0 = 0 |> not then 
            print_all_attributes ic oc head class_info;
        print_class ic oc tail;;

let rec print_type_file ic oc path = 
    let class_length = Hashtbl.length classes in 
    let sorted_path = List.fast_sort String.compare path in 
    fprintf oc "class_map\n%i\n" class_length;
    print_class ic oc sorted_path;
    fprintf oc "implementation_map\n%i\n" class_length;
    print_implementation ic oc sorted_path;
    fprintf oc "parent_map\n%i\n" (class_length - 1);
    print_parent oc sorted_path;
    seek_in ic 0;
    fprintf oc "%s\n" (input_line ic);
    print_annotated_ast ic oc (class_length - 5);
    close_in ic;
    close_out oc;;

(* 'Main' Method of the program. Calls all functions and returns either an error or a .cl-ast file ---------------------------------------------------------- *)
let main ic =
    match input_line ic with                                                                    (* Read in number of classes                   *)
    | "0"    -> raise No_class_main;                                                            (* If no class exists, cry no class Main       *)
    | length ->                                                                        
        add_classes ic (length |> int_of_string);                                               (* add_classes adds user classes superficially to list classes *)
        if Hashtbl.mem classes "Main" |> not then                                               (* Check if class Main exists*)
            raise (No_class_main);
        Hashtbl.iter (has_unknown_parent ic) classes;                                           (* Check all parents for any unknown classes   *)
        let path = topo_sort classes in                                                         (* Create a path for feature type checking     *)
        add_features ic path;                                                                 
        Hashtbl.iter (add_expressions ic) classes;
        print_type_file ic (String.concat "type" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out) path;;

(* Error handler - Calls main method and handles all errors encountered ------------------------------------------------------------------------------------- *)

let ic = open_in argv.(1) in (* Opens input file. will crash if file does not exist *)      
try main ic with
(* Class Exceptions *)
| No_class_main  ->
    printf "ERROR: 0: Type-Check: class Main not found\n"; 
| No_method_main ->
    printf "ERROR: 0: Type-Check: class Main method main not found\n";
| Method_main_has_parameters ->
    printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n";
| Class_inherits err  ->
    printf "ERROR: %s: Type-Check: class %s inherits from %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Class_redefine (location, name) ->
    printf "ERROR: %s: Type-Check: class %s redefined\n" location name;
| Class_self_type err              ->
    printf "ERROR: %s: Type-Check: class named SELF_TYPE\n" err
| Class_unknown  err  ->
    printf "ERROR: %s: Type-Check: class %s inherits from unknown class %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Cycle err           ->       
    printf "ERROR: 0: Type-Check: inheritance cycle: %s" err;
    print_cycle err (Hashtbl.find classes err).parent;                   
    printf "\n";

(* Feature Exceptions *)
| Attribute_redefine err->
    printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Attribute_self err    ->
    printf "ERROR: %s: Type-Check: class %s has an attribute named self\n"
        (List.hd err) (List.nth err 1)
| Attribute_unknown err ->
    printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3);
| Method_redefine err   ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_formal_duplicate err    ->
    printf "ERROR: %s: Type-Check: class %s has method test with duplicate formal parameter named %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_formal_length err       ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes number of formals\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Method_formal_redefine err     ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes type of formal %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3);
| Method_formal_self err        ->
    printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter named self\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_return_redefine err    ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes return type (from %s to %s)\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) (List.nth err 4);
| Method_unknown_formal_type err ->
    printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter of unknown type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3);
| Method_unknown_return_type err ->
    printf "ERROR: %s: Type-Check: class %s has method %s with unknown return type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)

(* Expression Exceptions *)
| Arithmetic_type err ->
    printf "ERROR: %s: Type-Check: arithmetic on %s %s instead of Ints\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Assign_self err ->
    printf "ERROR: %s: Type-Check: cannot asign to self\n" err
| Assign_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in assignment\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Attribute_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in initialized attribute\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Case_self err         ->
    printf "ERROR: %s: Type-Check: binding self in a case expression is not allowed\n" err
| Dispatch_unknown err ->
    printf "ERROR: %s: Type-Check: unknown method %s in dispatch on %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Dispatch_parameter_mismatch err ->
    printf "ERROR: %i: Type-Check: wrong number of actual arguments (%i vs. %i)\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Dispatch_parameter_nonconform err ->
    printf "ERROR: %s: Type-Check: argument %s type %s does not conform to formal type %s"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)
| Static_dispatch_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in static dispatch\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Equality_mismatch err ->
    printf "ERROR: %s: Type-Check: comparison between %s and %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Identifier_unbound err ->
    printf "ERROR: %s: Type-Check: unbound identifier %s\n"
        (List.hd err) (List.nth err 1)
| Let_nonconform err ->
    printf "ERROR %s: Type-Check: initializer type %s does not conform to type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Let_self err ->
    printf "ERROR: %s: Type-Check: binding self in a let is not allowed\n" err
| Method_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in method %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)
| Negate_type err ->
    printf "ERROR: %s: Type-Check: negate applied to type %s instead of Int\n"
        (List.hd err) (List.nth err 1)
| Not_type err ->
    printf "ERROR: %s; Type-Check: not applied to type %s instead of Bool\n"
        (List.hd err) (List.nth err 1)
| Unknown_type err -> 
    printf "ERROR: %s: Type-Check: unknown type %s\n"
        (List.hd err) (List.nth err 1)