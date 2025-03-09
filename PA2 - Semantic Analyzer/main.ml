open Printf;;
open Hashtbl;;
open Sys;;

(* Hi. This is my type-checker. It's a proper mess. But I'm proud of it. 
    Here is a bit of info on the program:

    The program is divided into a couple parts:
    1. Exception Definitions
    2. Datastructure Definitions
    3. Base Classes Initialization
    4. Topological Sort Algorithm
    5. Helper Functions
    6. Type-Checker
    7. Print Handling
    8. Main Method
    9. Error Handling

    Here is a brief overview of what each part does:
    1. Exception Definitions -
        Defines every single type error found in the COOL program. Is used to redirect any error to the end of the program.
    2. Datastructure Definitions - 
        Defines the data structures that will be used to read and type-check the AST. Contains basic functions as well.
    3. Base Classes Initialization -
        Adds the standard COOL classes that comes with every COOL program. Also creates the Hash table that will store all classes.
    4. Topological Sort Algorithm -
        Sorts all classes based on the inheritance tree. Class Object will always be the first class of the list
    5. Helper Functions -
        Minor functions that help the later parts of the program.
    6. Type-Checker -
        Probably the most important part of the program. Type-Checks classes, methods, and expressions in the AST.
    7. Print Handling - 
        Handles the creation of .cl-type file. The file is broken to four parts: class_map, implementation_map, parent_map, and the annotated AST
    8. Main Method -
        The method that calls every other function. Does very little work.
    9. Error Handling -
        Here lies all the stdout messages that displays what error has been encountered. 
*)

(* Exceptions - The beginning *)

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
exception Arithmetic_type      of string list;;
exception Assign_self          of string;;
exception Assign_nonconform    of string list;;
exception Attribute_nonconform of string list;;
exception Case_self            of string;;
exception Case_self_type       of string;;
exception Case_type_duplicate  of string list;;
exception Dispatch_unknown              of string list;;
exception Dispatch_parameter_mismatch   of int    list;;
exception Dispatch_parameter_nonconform of string list;;
exception Static_dispatch_nonconform    of string list;;
exception Equality_mismatch  of string list;;
exception Identifier_unbound of string list;;
exception Let_nonconform     of string list;;
exception Let_self           of string;;
exception Method_nonconform  of string list;;
exception Negate_type        of string list;;
exception Not_type           of string list;;
exception Unknown_type       of string list;;
exception Wrong_type         of string list;;

(* Data Structures - The record is to store a class's info and the hash tables are to store environments and classes ---------------------------------------- *)

(* Type object_id is the basic building block of the other two types. Object_id will represent variables, attributes, formals, and method types
   object_id attributes: 
    a_type     -> Pair of types in the format (static type, dynamic type).
    t_list     -> List containing the types of expressions within a method body, attribute init, or let init. Empty otherwise 
    ic_pointer -> Used to store the location of the object_id in the AST *)
type object_id = {
    obj_type   : string;
    t_list     : string list;
    ic_pointer : int;
};;

(* Type a_method contains all the important info of a specific instance of a method. 
   All classes will only contain defined methods and not its ancestor/parent methods 
    a_method attributes:
    method_name  -> Name of the method
    f_parameters -> List of formal parameters of the method. List.length is the formal length
    info         -> object_id containing the method type and expression types of the method body *)

type a_method = {
    f_parameters : object_id list;
    info         : object_id;
};; 

(* Type a_class, will be referred to a_class in functions, contains all of the important info of a class.
   The Hashtbl classes will contain the pairs (string, a_class), where string is the class name and a_class will be the class info 
   a_class attributes: 
    parent        -> The class's parent (either Object if no_inherits or the class it inherits)
    attribute_env -> Hash table containing the pairs (string, object_id), where string is the attribute name and object_id will be the attribute's info
    method_env    -> List of methods defined in the class's features
    ic_pointers   -> A pointer to the class's name in the AST    
*)

type a_class = {
    parent        : string;
    attribute_env : (string, object_id) Hashtbl.t;
    method_env    : (string, a_method) Hashtbl.t;
    ic_pointers   : int * int;
};;

(* New Struct Functions *)

(* Given a type, returns a new object_id *)
let new_object_id t point = {obj_type = t; t_list = []; ic_pointer = point};;

(* Creates a new_method given a name, type, and list of formals *)
let new_a_method t args point = {info = new_object_id t point; f_parameters = List.map (fun arg -> new_object_id arg 0) args};;

(* Placeholder for find_object function. Represents a NULL value for objects. *)

let empty_object = new_object_id "" 0;; 

(* Placeholder for find_method function. Represents a NULL value for methods *)
let empty_method = new_a_method "" [] 0;;

(* Base Classes - Stores all the info about COOL's standard classes. Contains empty hashes for later use too ------------------------------------------------ *)

(* these hashes are used as placeholders for the base classes. *)
let empty_a_hash = Hashtbl.create 0;;     
let empty_m_hash = Hashtbl.create 0;;

(* Here is the method initialization of the base classes of COOL *)
let m_obj_hash = Hashtbl.create 50;;
Hashtbl.add m_obj_hash "abort"     (new_a_method "Object"    [] 0);;
Hashtbl.add m_obj_hash "copy"      (new_a_method "SELF_TYPE" [] 1);;
Hashtbl.add m_obj_hash "type_name" (new_a_method "String"    [] 2);;
let m_io_hash = Hashtbl.create 50;; 
Hashtbl.add m_io_hash "in_int"     (new_a_method "Int"    [] 0);;
Hashtbl.add m_io_hash "in_string"  (new_a_method "String" [] 1);;
Hashtbl.add m_io_hash "out_int"    (new_a_method "SELF_TYPE" ["Int"]    2);;
Hashtbl.add m_io_hash "out_string" (new_a_method "SELF_TYPE" ["String"] 3);;
let m_str_hash = Hashtbl.create 50;;
Hashtbl.add m_str_hash "concat" (new_a_method "String" ["String"] 0);;
Hashtbl.add m_str_hash "length" (new_a_method "Int"    [] 1);;
Hashtbl.add m_str_hash "substr" (new_a_method "String" ["Int"; "Int"] 2);;

(* Hashtbl classes will contain all of the classes within the COOL program. Class Object has the special parent "", which will be ignored when
    performing the topological sort later. *)
let classes = Hashtbl.create 50;;
let () = Hashtbl.add classes "Object" {parent = "";       ic_pointers = 0,0; attribute_env = empty_a_hash; method_env = m_obj_hash  };;
let () = Hashtbl.add classes "IO"     {parent = "Object"; ic_pointers = 0,0; attribute_env = empty_a_hash; method_env = m_io_hash   };;
let () = Hashtbl.add classes "Int"    {parent = "Object"; ic_pointers = 0,0; attribute_env = empty_a_hash; method_env = empty_m_hash};;
let () = Hashtbl.add classes "String" {parent = "Object"; ic_pointers = 0,0; attribute_env = empty_a_hash; method_env = m_str_hash  };;
let () = Hashtbl.add classes "Bool"   {parent = "Object"; ic_pointers = 0,0; attribute_env = empty_a_hash; method_env = empty_m_hash};;

(*  Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

(* Adds a parent to child edge to a list that will be sorted by topological sort *)
let create_edge class_name a_class edges = [a_class.parent; class_name] :: edges;;

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

(* Helper Functions ----------------------------------------------------------------------------------------------------------------------------------------- *)

(* Class Functions *)

(* Checks if class's parent is defined in the program. Raise Class_unknown error if it is not class Object and parent is undefined *)
let has_unknown_parent ic class_name a_class =
    if String.equal class_name "Object" |> not && Hashtbl.mem classes a_class.parent |> not then  
        let () = seek_in ic (fst a_class.ic_pointers) in
        let _ = input_line ic, input_line ic, input_line ic in
        raise (Class_unknown [input_line ic; class_name; a_class.parent]);;

(* Least Upper Bound Function - Computes the common ancestor class of two classes. Will always return a class hopefully. *)

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

(* Checks whether a class (c_type) is a child of another class (p_type). Returns a boolean of the c_type's legitamacy of lineage 
   Special Cases: 
   p_type is SELF_TYPE -> c_type must be SELF_TYPE. 
   c_type is SELF_TYPE -> The class c_type is referring to must conform to p_type. (This will be class_name)
*)
let rec conforms_to_body p_type c_type = 
    if String.equal c_type "Object" |> not && String.equal c_type p_type |> not then
        conforms_to_body p_type (Hashtbl.find classes c_type).parent
    else String.equal c_type p_type;;
let conforms_to class_name p_type c_type =
    if String.equal p_type "SELF_TYPE" then
        String.equal p_type c_type
    else if String.equal c_type "SELF_TYPE" then 
        conforms_to_body p_type class_name
    else conforms_to_body p_type c_type;;

(* Least Upper Bound Function : Returns the closest ancestor of two classes *)
let lub self_class class_1 class_2 =
    if String.equal class_1 class_2 then class_1
    else
        let class_1 = if String.equal "SELF_TYPE" class_1 then self_class else class_1 in
        let class_2 = if String.equal "SELF_TYPE" class_2 then self_class else class_2 in
        let class_1_ancestors = get_ancestor_tree class_1 |> List.rev in
        let class_2_ancestors = get_ancestor_tree class_2 |> List.rev in
        find_common_ancestor class_1_ancestors class_2_ancestors "Object";;

(* Object Functions *)

(* Checks whether an attribute has been defined within one of the class's ancestors. Return boolean of its existence *)
let rec attribute_has_ancestor identifier child_name child_info =
    match child_name with
    | "Object" -> false
    | _        -> 
        if Hashtbl.mem child_info.attribute_env identifier then true
        else attribute_has_ancestor identifier child_info.parent (Hashtbl.find classes child_info.parent);;

let rec find_object identifier a_class =
    match Hashtbl.find_opt a_class.attribute_env identifier with
    | Some object_id -> object_id;
    | None ->
        if String.equal a_class.parent "" then
            empty_object
        else find_object identifier (Hashtbl.find classes a_class.parent);;

(* Returns a list that contains all of the defined and inherited attributes of a class. 
    The attributes are sorted by: 
        1) Class with Object being first followed by its children
        2) The order in which the attribute is defined; first defined -> first in list, last defined -> last in list
*)
let rec get_attribute_list a_class = 
    let class_attrs = Hashtbl.fold (fun k e acc -> e :: acc) a_class.attribute_env [] in
    let class_attrs = List.fast_sort (fun e1 e2 -> e1.ic_pointer - e2.ic_pointer) class_attrs in 
    if String.equal a_class.parent "" |> not then
        get_attribute_list (Hashtbl.find classes a_class.parent) @ class_attrs
    else class_attrs;;

(* Method Functions *)

(* Checks if a method has been already declared before. Checks parents as well. Returns the method or the empty_method defined under Method Functions *)
let rec find_method identifier a_class =
    match Hashtbl.find_opt a_class.method_env identifier with
    | Some a_method -> a_method;
    | None -> 
        if String.equal a_class.parent "" then
            empty_method
        else find_method identifier (Hashtbl.find classes a_class.parent);;

(* Returns a list that contains all of the defined and inherited methods of a class. 
    The methods are sorted by: 
        1) Class with Object being first followed by its children
        2) The order in which the methods is defined; first defined -> first in list, last defined -> last in list
    Overridden methods will hide its parent method. 
    Each element of the list being returned contains the class the method was defined in and the method name 
*)
let get_snd tuple = match tuple with (i, j, k) -> j;;
let get_trd tuple = match tuple with (i, j, k) -> k;;

let rec add_or_replace insert lst = 
    match lst with
    | [] -> insert
    | head::tail ->
        let found = List.find_opt (fun ele -> String.equal (get_snd head) (get_snd ele)) insert in
        if Option.is_none found then
            head :: add_or_replace insert tail
        else
            Option.get found :: add_or_replace (List.filter (fun e -> e = Option.get found |> not) insert) tail;;

let rec get_method_list class_name a_class =
    let class_methods = Hashtbl.fold (fun k e acc -> (class_name, k, e) :: acc) a_class.method_env [] in
    let class_methods = List.fast_sort (fun e1 e2 -> (get_trd e1).info.ic_pointer - (get_trd e2).info.ic_pointer) class_methods in
    match class_name with
    | "Object" -> class_methods;
    | _ -> add_or_replace class_methods (get_method_list a_class.parent (Hashtbl.find classes a_class.parent));;
    
(* Fringe Error handling *)

(* Checks if a zero parameter method called main is defined in class Main. 
    Raises No_method_main if no main method is found or Method_main_has_parameters if there are parameters in main *)
let handle_main a_class = 
    let m = find_method "main" a_class in
    if m = empty_method then
        raise (No_method_main);
    if m.f_parameters != [] then
        raise (Method_main_has_parameters);;

(* A function that prints a cycle if the topological sort algorithm raises the Cycle error. 
    Works by recursively calling a method's parents until the method is encountered again *)
let rec print_cycle start curr =
    if String.equal start curr |> not then
        let () = printf " %s" curr in
        print_cycle start (Hashtbl.find classes curr).parent;;


(* Methods for Skipping parts of the AST ------------------------------------------------------------------------------------------------------------------- *)

(* Expression skip - Follows the structure of the AST when it comes to expressions
    Here what it is does according to each keyword encountered.
    assign -> read identifier and its location and call expressions_skip.
    block  -> call skip_block, which calls expression_skip equivalent to the number of expressions in the block
    case   -> read expr0 using expressions_skip, read number of branches and call skip_case

    dynamic_dispatch -> read type using expressions_skip, read identifier and its location and call skip_dispatch while reading # of args
    self_dispatch    -> read identifier and its location and call skip_dispatch while reading # of args
    static_dispatch  -> read type using expressions_skip, read method, parent, and its locations; call skip_dispatch while reading # of args

    Artihmetic and Boolean comparisons -> call expressions_skip twice
    Identifier and new -> read from AST twice. (Ident reads its name and location while new reads class name and location)
    if -> read expressions_skip three times for conditional, then expression, and else expression
    integer and string -> read integer or string
    
    isvoid, negate, and not -> read expression_skip. Each keyword relies on another expression to function.
    let -> Call skip_let_binding and read let body using expressions_skip
    true and false -> Do nothing.

    Special functions:
    skip_block -> Read each expression from block

*)
let rec expressions_skip ic =
    ignore (input_line ic); 
    let keyword = input_line ic in
    match keyword with
    | "assign" -> 
        let _ = input_line ic, input_line ic in 
        expressions_skip ic
    | "block" -> 
        let block_length = input_line ic |> int_of_string in
        skip_block ic block_length;
    | "case"  ->
        let () = expressions_skip ic in
        let case_length = input_line ic |> int_of_string in
        skip_case ic case_length;
    | "dynamic_dispatch" ->
        let () = expressions_skip ic in 
        let _ = input_line ic, input_line ic in 
        skip_dispatch ic (input_line ic |> int_of_string);
    | "self_dispatch" ->  
        let _ = input_line ic, input_line ic in skip_dispatch ic (input_line ic |> int_of_string)
    | "static_dispatch" -> 
        let () = expressions_skip ic in 
        let _ = input_line ic, input_line ic, input_line ic, input_line ic in
        skip_dispatch ic (input_line ic |> int_of_string);
    | "eq" | "lt" | "le" | "plus" | "minus" | "times" | "divide" | "while" -> 
        expressions_skip ic;  expressions_skip ic;
    | "identifier" | "new" -> 
        let _ = input_line ic, input_line ic in () 
    | "if" -> 
        expressions_skip ic; expressions_skip ic; expressions_skip ic;
    | "integer" | "string" -> 
        let _ = input_line ic in ();
    | "isvoid" | "negate" | "not" -> expressions_skip ic;
    | "let" ->  
        skip_let_binding ic (input_line ic |> int_of_string); expressions_skip ic;
    | "true" | "false" -> ();
    | str -> raise (Failure "expression_skip failed");
and skip_block ic length =
    match length with 
    | 0 -> ()
    | _ ->  expressions_skip ic; skip_block ic (length - 1);
and skip_case ic length = 
    match length with
    | 0 -> ()
    | _ ->
        let _ = input_line ic, input_line ic, input_line ic, input_line ic in
        expressions_skip ic; skip_case ic (length - 1);
and skip_dispatch ic length = 
    match length with
    | 0 -> ();
    | _ -> expressions_skip ic; skip_dispatch ic (length - 1)
and skip_let_binding ic length = 
    match length with
    | 0 -> ();
    | _ ->
        let binding = input_line ic in 
        let _ = input_line ic, input_line ic, input_line ic, input_line ic in
        if String.equal binding "let_binding_init" then
            expressions_skip ic;
        skip_let_binding ic (length - 1);; 
(* When skipping features is necessary. Calls expressions_skip according to the number of features in the class being read *)
let rec features_skip ic length =
    match length with 
    | 0 -> ()
    | _ -> 
        let () = match input_line ic with
        | "attribute_init" ->
            let _ = input_line ic, input_line ic, input_line ic, input_line ic in expressions_skip ic;
        | "attribute_no_init" -> 
            let _ = input_line ic, input_line ic, input_line ic, input_line ic in ();
        | "method" ->
            let _ = input_line ic, input_line ic in
            for i = 1 to input_line ic |> int_of_string do
                let _ = input_line ic, input_line ic, input_line ic, input_line ic in ();
            done;
            let _ = input_line ic, input_line ic in
            expressions_skip ic;
        | _ -> raise (Failure "features_skip failed\n")
        in features_skip ic (length - 1);;

(* AST Tree Parser Functions -------------------------------------------------------------------------------------------------------------------------------- *)

(* Reads either an attribute initialization or a method body
    Returns a list of dynamic types. Used for storing the types of the expression found 
    The structure of read_expression is the same as expression_skip
    Here is what read_expression checks:
    assign -> Checks if variable is called self. Checks if variable exists. Checks if type exists. Checks if type conforms to variable static type
    block  -> No checking
    case   -> For each variable in branch, check if its called self, check if type is SELF_TYPE, check if type exists, check if type is duplicated, and finds
        least upper bound of all branches
    dynamic_dispatch -> Checks if method exists. Checks if argument length matches with formal length. Checks if arguments conform to formal
    self_dispatch    -> Does the exact same checkings as dynamic_dispatch
    static_dispatch  -> Checks if parent exists. The rest is the same as dynamic_dispatch
    eq, lt, le -> Compare both sides and check if they are equal.
    identifier -> Check if name is self and check if variable exists.
    if         -> Checks conditional expression if its dynamic type is bool
    integer, string, isvoid, true, false -> No type checking
    let -> For each let binding, check if variable is called self. Check if let binding is SELF_TYPE. If binding has an initializer, check dynamic type
        conforms to static type
    negate -> Check if dynamic type is Bool
    new -> Check if type/class exists
    Arithmetic Operations -> Check if both sides are Int
    while -> Check if conditional is boolean
*)
let rec read_expression ic class_name a_class =                                                 
    let program_line = input_line ic in
    match input_line ic with
    | "assign" -> 
        let (* ident. loc *) _ = input_line ic in 
        let identifier = input_line ic in
        if String.equal identifier "self" then
            raise (Assign_self program_line);
        let object_id = find_object identifier a_class in
        if object_id = empty_object then 
            raise (Unknown_type [program_line; identifier]);
        let init_type = read_expression ic class_name a_class in
        if conforms_to class_name (object_id.obj_type) (List.hd init_type) |> not then
            raise (Assign_nonconform [program_line; List.hd init_type; object_id.obj_type]);
        List.hd init_type :: init_type
    | "block" -> 
        let block_type = ref "" in 
        let type_list = block_type_check ic class_name a_class block_type (input_line ic |> int_of_string) in
        !block_type :: type_list
    | "case"  ->
        let case_expr = read_expression ic class_name a_class in 
        let case_type = ref "" in
        let case_list = case_type_check [] ic class_name a_class case_type (input_line ic |> int_of_string (* # of arguments *)) in
        !case_type :: case_expr @ case_list;
    | "dynamic_dispatch" ->
        let ident_list = read_expression ic class_name a_class in 
        let method_loc = input_line ic in let method_name  = input_line ic in 
        let method_type = if String.equal (List.hd ident_list) "SELF_TYPE" then class_name else List.hd ident_list in
        let a_method = find_method method_name (Hashtbl.find classes method_type) in 
        if a_method = empty_method then
            raise (Dispatch_unknown [method_loc; method_name; List.hd ident_list]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        if String.equal (a_method.info.obj_type) "SELF_TYPE" then
            List.hd ident_list :: ident_list @ (dispatch_type_check ic class_name a_class a_method.f_parameters program_line 1 argument_length)
        else
            a_method.info.obj_type :: ident_list @ (dispatch_type_check ic class_name a_class a_method.f_parameters program_line 1 argument_length);
    | "self_dispatch" ->  
        let method_loc = input_line ic in let method_name = input_line ic in 
        let a_method = find_method method_name a_class in
        if a_method = empty_method then 
            raise (Dispatch_unknown [method_loc; method_name; class_name]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        a_method.info.obj_type :: (dispatch_type_check ic class_name a_class a_method.f_parameters program_line 1 argument_length);
    | "static_dispatch" -> 
        let ident_list = read_expression ic class_name a_class in 
        let (* parent_loc *) _ = input_line ic in let parent = input_line ic in 
        if conforms_to parent parent (List.hd ident_list) |> not then
            raise (Static_dispatch_nonconform [program_line; List.hd ident_list; parent]);
        let method_loc = input_line ic in let method_name  = input_line ic in 
        let a_method = find_method method_name (Hashtbl.find classes parent) in 
        if a_method = empty_method then
            raise (Dispatch_unknown [method_loc; method_name; parent]);
        let argument_length = input_line ic |> int_of_string  in
        let formal_length = List.length a_method.f_parameters in
        if argument_length != formal_length then 
            raise (Dispatch_parameter_mismatch [int_of_string method_loc; argument_length; formal_length]);
        if String.equal a_method.info.obj_type "SELF_TYPE" then
            List.hd ident_list :: ident_list @ (dispatch_type_check ic class_name a_class a_method.f_parameters program_line 1 argument_length)
        else
            a_method.info.obj_type :: ident_list @ (dispatch_type_check ic class_name a_class a_method.f_parameters program_line 1 argument_length);
    | "eq" | "lt" | "le" -> 
        let left_type = read_expression ic class_name a_class in let right_type = read_expression ic class_name a_class in
        if String.equal (List.hd left_type) (List.hd right_type) |> not then
            raise (Equality_mismatch [program_line; List.hd left_type; List.hd right_type]);
        "Bool" :: left_type @ right_type;
    | "identifier" -> 
        let (* ident_loc *) _ = input_line ic in let identifier = input_line ic in 
        if String.equal identifier "self" then
            ["SELF_TYPE"]
        else
            let object_id = find_object identifier a_class in
            if object_id = empty_object then
                raise (Identifier_unbound [program_line; identifier])
            else [object_id.obj_type]; 
    | "if" -> 
        let cond_type = read_expression ic class_name a_class in
        if String.equal (List.hd cond_type) "Bool" |> not then
            raise (Wrong_type [program_line; "conditional"; List.hd cond_type]);
        let then_type = read_expression ic class_name a_class in 
        let else_type = read_expression ic class_name a_class in 
        lub class_name (List.hd then_type) (List.hd else_type) :: (cond_type @ then_type @ else_type) 
    | "integer" -> let (* integer *) _ =  input_line ic in ["Int"];
    | "isvoid"  -> "Bool" :: read_expression ic class_name a_class;
    | "let" ->  
        let_binding_type_check ic class_name a_class program_line [] [] (input_line ic |> int_of_string (* # of bindings *))
    | "negate" -> 
        let negate_list = read_expression ic class_name a_class in
        if String.equal (List.hd negate_list) "Int" |> not then
            raise (Negate_type [program_line; List.hd negate_list]);
        "Int" :: negate_list;
    | "new" -> 
        let class_loc = input_line ic in let new_class = input_line ic in 
        if String.equal new_class "SELF_TYPE" then 
            ["SELF_TYPE"]        
        else if Hashtbl.mem classes new_class |> not then 
            raise (Unknown_type [class_loc; new_class]) 
        else [new_class];         
    | "not" -> 
        let not_list = read_expression ic class_name a_class in
        if String.equal (List.hd not_list) "Bool" |> not then 
            raise (Not_type [program_line; List.hd not_list]);
        "Bool" :: not_list;
    | "plus" | "minus" | "times" | "divide" -> 
        let left_type = read_expression ic class_name a_class in let right_type = read_expression ic class_name a_class in 
        if String.equal (List.hd left_type) "Int" && String.equal (List.hd right_type) "Int" then 
            "Int" :: (left_type @ right_type)
        else raise (Arithmetic_type [program_line; List.hd left_type; List.hd right_type]);
    | "string" -> let (* string *) _ = input_line ic in ["String"];
    | "true" | "false" -> ["Bool"];
    | "while"  -> 
        let cond_type = read_expression ic class_name a_class in 
        if String.equal (List.hd cond_type) "Bool" |> not then
            raise (Wrong_type [program_line; "predicate"; List.hd cond_type]);
        let body_type = read_expression ic class_name a_class in "Object" :: cond_type @ body_type;
    | str -> raise (Failure "read_expression failed") 
and block_type_check ic class_name a_class block_type length =
    match length with
    | 0 -> block_type := "Object"; [];
    | 1 -> 
        let type_list = read_expression ic class_name a_class in
        block_type := List.hd type_list;
        type_list;
    | _ -> 
        let type_list = read_expression ic class_name a_class in
        type_list @ block_type_check ic class_name a_class block_type (length - 1);
and case_type_check acc ic class_name a_class case_type length = 
    match length with
    | 0 -> []
    | _ ->
        let ident_loc = input_line ic in let branch_ident = input_line ic in
        let type_loc = input_line ic in let branch_type = input_line ic in
        if String.equal branch_ident "self" then
            raise (Case_self ident_loc);
        if String.equal branch_type "SELF_TYPE" then
            raise (Case_self_type type_loc);
        if Hashtbl.mem classes branch_type |> not then
            raise (Unknown_type [ident_loc; branch_type]);
        if List.mem branch_type acc then
            raise (Case_type_duplicate [ident_loc; branch_type]);
        Hashtbl.add a_class.attribute_env branch_ident (new_object_id branch_type 0);
        let case_body = read_expression ic class_name a_class in 
        Hashtbl.remove a_class.attribute_env branch_ident;
        if String.equal !case_type "" then
            case_type := branch_type
        else
            case_type := lub class_name !case_type branch_type;
        case_body @ case_type_check (branch_type :: acc) ic class_name a_class case_type (length - 1)
and dispatch_type_check ic class_name a_class formal_types program_line step length = 
    match length with 
    | 0 -> []
    | _ -> let arg_type = read_expression ic class_name a_class in
        if conforms_to class_name (List.hd formal_types).obj_type (List.hd arg_type) |> not then
            raise (Dispatch_parameter_nonconform [program_line; step |> string_of_int; List.hd arg_type; (List.hd formal_types).obj_type]);
        arg_type @ dispatch_type_check ic class_name a_class (List.tl formal_types) program_line (step + 1) (length - 1);
and let_binding_type_check ic class_name a_class program_line ident_list type_list length = 
    match length with
    | 0 ->  let let_body = read_expression ic class_name a_class in
            List.iter (Hashtbl.remove a_class.attribute_env) ident_list;
            (List.hd let_body) :: type_list @ let_body
    | _ ->
        let binding = input_line ic in 
        let ident_loc  = input_line ic in let identifier = input_line ic in
        let (* type_loc  *) _ = input_line ic in let bind_type  = input_line ic in
        if String.equal identifier "self" then
            raise (Let_self ident_loc);
        if String.equal bind_type "SELF_TYPE" |> not && Hashtbl.mem classes bind_type |> not then
            raise (Unknown_type [program_line; bind_type]);
        Hashtbl.add a_class.attribute_env identifier (new_object_id bind_type 0);
        let ident_list = identifier :: ident_list in
        let type_list = 
            if String.equal binding "let_binding_init" then
                let init_type = read_expression ic class_name a_class in
                if conforms_to class_name bind_type (List.hd init_type) |> not then
                    raise (Let_nonconform [program_line; List.hd init_type; bind_type]);
                (type_list @ List.hd init_type :: init_type)
            else type_list @ [bind_type]
        in let_binding_type_check ic class_name a_class program_line ident_list type_list (length - 1);;

(* type_check_features reads the expressions of attribute initializers and method bodies.
    Three cases four type_check_features
    attribute_no_init -> Skip attribute
    attribute_init    -> Read initialization using read_expression, then check if dynamic type (from read_expression) conforms to 
        static type (from attribute_env). Then replaces old attributes with new attributes that contain all of the dynamic types of the initialization
    method -> Add formals to attribute_env using initialize formals and call read_expression to get dynamic type. Check if dynamic type conforms to 
        static type and afterwards remove formals from attribute_env. Replace old method with new method.
*)
let rec initialize_formals ic a_class length = 
    match length with 
    | 0 -> []
    | _ ->
        let (* ident location *) _ = input_line ic in let identifier  = input_line ic in
        let (* type location  *) _ = input_line ic in let formal_type = input_line ic in 
        Hashtbl.add a_class.attribute_env identifier (new_object_id formal_type 0);
        identifier :: initialize_formals ic a_class (length - 1);;

let rec type_check_features ic class_name a_class length =
    match length with
    | 0 -> ()
    | _ ->
        let feature    = input_line ic in
        let ident_loc  = input_line ic in 
        let identifier = input_line ic in
        let () = match feature with
        | "attribute_no_init" -> 
            ignore (input_line ic, input_line ic);
        | "attribute_init"    ->
            let _ = (input_line ic), (input_line ic) in
            let a_attr = Hashtbl.find a_class.attribute_env identifier in
            let type_list = read_expression ic class_name a_class in 
            if conforms_to class_name a_attr.obj_type (List.hd type_list) |> not then
                raise (Attribute_nonconform [ident_loc; List.hd type_list; a_attr.obj_type]);
            Hashtbl.replace a_class.attribute_env identifier {a_attr with t_list = type_list};
        | "method" ->
            let a_method = find_method identifier a_class in
            let formal_list = initialize_formals ic a_class (input_line ic |> int_of_string) in 
            let _  = input_line ic, input_line ic in (* loc and method_type*)
            let type_list = read_expression ic class_name a_class in
            if conforms_to class_name a_method.info.obj_type (List.hd type_list) |> not then
                raise (Method_nonconform [ident_loc; List.hd type_list; a_method.info.obj_type; identifier]);
            List.iter (fun e -> Hashtbl.remove a_class.attribute_env e) formal_list;
            Hashtbl.replace a_class.method_env identifier {a_method with info = {a_method.info with t_list = type_list}} ;
        | _ -> raise (Failure "initialize_features failed")
        in type_check_features ic class_name a_class (length - 1);;

let add_expressions ic class_name a_class =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> ()
    | _ ->
        seek_in ic (snd a_class.ic_pointers);
        type_check_features ic class_name a_class (input_line ic |> int_of_string);;

(* read_features initializes all of the features within a class without type-checking any of the expressions within the features.
    There are two cases:
    attribute_no_init/attribute_init ->
        Check if attribute is called self, check if attribute has already been defined, and check if type exists. If all is satisfied add attribute to
            attribute_env.
    method ->
        Check if class already defined method. 
        Then for each formal, check if they aren't called self, if they have all unique names, and if the formal type exists. (This is done in new_formals)
        Now if a method was already defined in a parent class, then check if the formal length is the same, and if the types of each formal match.
            (This is done in compare_formals)
*)
let rec compare_formals_body ident_and_loc prev_formals new_formals =
    match ident_and_loc with
    | [("", r_loc)] -> 
        if String.equal (fst prev_formals) (fst new_formals) |> not then
            raise (Method_return_redefine [r_loc; fst prev_formals; fst new_formals]);
    | (ident, i_loc) :: tail ->
        let prev_formal = (snd prev_formals) |> List.hd in let new_formal = (snd new_formals) |> List.hd in
        if String.equal (prev_formal.obj_type) (new_formal.obj_type) |> not then 
            raise (Method_formal_redefine [i_loc; ident])
        else compare_formals_body tail (fst prev_formals, snd prev_formals |> List.tl) (fst new_formals, snd new_formals |> List.tl)
    | [] -> raise (Failure "compare_formals_body is bad")
let compare_formals loc_and_ident prev_method new_method =
    compare_formals_body loc_and_ident (prev_method.info.obj_type, prev_method.f_parameters) (new_method.info.obj_type, new_method.f_parameters);;

let rec new_formals_body ic ic_pointer formal_list extra_info length = 
    match length with
    | 0 -> 
        let type_location = input_line ic in let return_type = input_line ic in 
        if String.equal return_type "SELF_TYPE" |> not && Hashtbl.mem classes return_type |> not then
            raise (Method_unknown_return_type [type_location; return_type]);
        new_a_method return_type (List.rev formal_list) ic_pointer, (("", type_location) :: extra_info |> List.rev);
    | _ -> 
        let identifier_location = input_line ic in let identifier  = input_line ic in
        let type_location       = input_line ic in let formal_type = input_line ic in
        if String.equal identifier "self" 
            then raise (Method_formal_self [identifier_location]);
        if List.exists (fun ele -> String.equal (fst ele) identifier) extra_info 
            then raise (Method_formal_duplicate [identifier_location; identifier]);
        if Hashtbl.mem  classes formal_type |> not then 
            raise (Method_unknown_formal_type [type_location; formal_type])
        else new_formals_body ic ic_pointer (formal_type :: formal_list) ((identifier, type_location) :: extra_info) (length - 1);;
let new_formals ic ic_pointer length = 
    new_formals_body ic ic_pointer [] [] length;;

let rec read_features ic class_name a_class length = 
    match length with
    | 0 -> ();
    | _ -> 
        let feature        = input_line ic in
        let ident_location = input_line ic in
        let ic_pointer     = pos_in     ic in
        let identifier     = input_line ic in
        let () = match feature with
        | "attribute_init" | "attribute_no_init" ->
            if String.equal identifier "self" then 
                raise (Attribute_self [ident_location; class_name]);
            if attribute_has_ancestor identifier class_name a_class then
                raise (Attribute_redefine [ident_location; class_name; identifier]);
            let type_location  = input_line ic in
            let type_attribute = input_line ic in
            if String.equal type_attribute "SELF_TYPE" |> not && Hashtbl.mem classes type_attribute |> not then
                raise (Attribute_unknown [type_location; class_name; identifier; type_attribute]);
            Hashtbl.add a_class.attribute_env identifier (new_object_id type_attribute ic_pointer);
            if feature = "attribute_init" then
                let _ = expressions_skip ic in ();
        | "method" -> 
            if Hashtbl.mem a_class.method_env identifier then
                raise (Method_redefine [ident_location; class_name; identifier]);
            let formals_length  = input_line ic |> int_of_string in
            let prev_method = find_method identifier (Hashtbl.find classes a_class.parent) in
            let method_info = try new_formals ic ic_pointer formals_length with
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
            in Hashtbl.add a_class.method_env identifier (fst method_info); 
                expressions_skip ic;
        | _ -> raise (Failure "read_features failed:")
        in read_features ic class_name a_class (length - 1);;

(* Adds features to every class defined in the program. This is done by calling read_features for each class 
    The class Main has a special case where method main must be defined somewhere in main or in one of its parents 
*)
let rec add_features ic path =
    match path with
    | [] -> ();
    | head::tail ->
        if List.mem head ["Bool"; "IO"; "Int"; "Object"; "String"] |> not then
            let a_class = Hashtbl.find classes head in
            let () = seek_in ic (snd a_class.ic_pointers) in
            let () = read_features ic head a_class (input_line ic |> int_of_string) in
            if String.equal head "Main" then 
                handle_main (Hashtbl.find classes head);
        else ();
        add_features ic tail;;

(* Adds classes to to Hashtbl classes. Checks if class name isn't SELF_TYPE and if it inherits, whether the parent class is either Bool, Int, or String *)
let rec add_classes ic length =
    match length with
    | 0 -> ();
    | _ ->
        let class_pointer = pos_in ic     in
        let program_line  = input_line ic in
        let class_name    = input_line ic in
        if String.equal class_name "SELF_TYPE" then
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
        let a_class = {
            parent        = fst class_inheritance;
            attribute_env = Hashtbl.create 50;
            method_env    = Hashtbl.create 50;
            ic_pointers   = (class_pointer, feature_pointer);
        } in Hashtbl.add classes class_name a_class;
        add_classes ic (length - 1);;

(* Print to .cl-ast Functions - There are four main components: class_map, implementation_map, parent_map, and the annotated AST --------------------------- *)
(* print_expressions follows the same structure as expression_skip. This method simply prints out the type right after the program line of the expression *)
let rec print_expressions ic oc type_list =
    fprintf oc "%s\n" (input_line ic);                                   
    let keyword = input_line ic in
    fprintf oc "%s\n%s\n" (List.hd type_list) keyword;
    let type_list = List.tl type_list in
    match keyword with
    | "assign" -> 
        fprintf oc "%s\n" (input_line ic);  
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
        fprintf oc "%s\n" (input_line ic);     
        fprintf oc "%s\n" (input_line ic);
        let arg_length = input_line ic |> int_of_string in
        fprintf oc "%i\n" arg_length;
        print_dispatch ic oc type_list arg_length
    | "self_dispatch" ->  
        fprintf oc "%s\n" (input_line ic);       
        fprintf oc "%s\n" (input_line ic);
        let arg_length = input_line ic |> int_of_string in
        fprintf oc "%i\n" arg_length;
        print_dispatch ic oc type_list arg_length
    | "static_dispatch" -> 
        let type_list = print_expressions ic oc type_list in 
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic); 
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
        let arg_length = input_line ic |> int_of_string in
        fprintf oc "%i\n" arg_length;
        print_dispatch ic oc type_list arg_length
    | "eq" | "lt" | "le" | "plus" | "minus" | "times" | "divide" | "while" -> 
        print_expressions ic oc type_list |>  print_expressions ic oc;
    | "identifier" -> 
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic);
        type_list;
    | "if" -> 
        print_expressions ic oc type_list |> print_expressions ic oc |> print_expressions ic oc
    | "integer" | "string" -> 
        fprintf oc "%s\n" (input_line ic);
        type_list;
    | "isvoid" | "negate" | "not" -> 
        print_expressions ic oc type_list;
    | "let" ->  
        let let_length = input_line ic |> int_of_string in (* # of bindings *)
        fprintf oc "%i\n" let_length;
        print_let_binding ic oc type_list let_length |> print_expressions ic oc
    | "new" -> 
        fprintf oc "%s\n" (input_line ic);                               
        fprintf oc "%s\n" (input_line ic);
        type_list;
    | "true" | "false" -> type_list;
    | str -> raise (Failure "print_expression failed");
and print_block ic oc type_list length =
    match length with 
    | 0 -> type_list
    | _ -> let type_list = print_expressions ic oc type_list in print_block ic oc type_list (length - 1)
and print_case ic oc type_list length = 
    match length with
    | 0 -> type_list
    | _ ->
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
        let type_list = print_expressions ic oc type_list in
        print_case ic oc type_list (length - 1);
and print_dispatch ic oc type_list length = 
    match length with
    | 0 -> type_list;
    | _ -> 
        let type_list = print_expressions ic oc type_list in
        print_dispatch ic oc type_list (length - 1)
and print_let_binding ic oc type_list length = 
    match length with
    | 0 -> type_list;
    | _ ->
        let binding = input_line ic in 
        fprintf oc "%s\n" binding;
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
        fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (List.hd type_list);
        let _ = input_line ic in let type_list = List.tl type_list in
        if String.equal binding "let_binding_init" then
            print_let_binding ic oc (print_expressions ic oc type_list) (length - 1)
        else print_let_binding ic oc type_list (length - 1);;

let rec print_features ic oc a_class length =
    match length with
    | 0 -> () 
    | _ -> 
        let feature = input_line ic in
        fprintf oc "%s\n%s\n" feature (input_line ic); (* Program line of identifier *)
        let identifier = input_line ic in
        fprintf oc "%s\n" identifier;
        let () = match feature with
        | "attribute_init" | "attribute_no_init" -> 
            fprintf oc "%s\n" (input_line ic);
            fprintf oc "%s\n" (input_line ic); (* Program line of type and the type of attribute *)
            if String.equal feature "attribute_init" then
                ignore (print_expressions ic oc (Hashtbl.find a_class.attribute_env identifier).t_list);
        | "method" -> 
            let a_method = Hashtbl.find a_class.method_env identifier in
            fprintf oc "%s\n" (input_line ic);
            for i = 1 to List.length a_method.f_parameters do 
                fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);                               
                fprintf oc "%s\n" (input_line ic); fprintf oc "%s\n" (input_line ic);
            done;
            fprintf oc "%s\n" (input_line ic);
            fprintf oc "%s\n" (input_line ic);
            ignore (print_expressions ic oc a_method.info.t_list);
        | _ -> raise (Failure "Print_features read something that wasn't a feature")
        in print_features ic oc a_class (length - 1);;

let rec print_annotated_ast ic oc length = 
    match length with
    | 0 -> ()
    | _ -> 
        fprintf oc "%s\n" (input_line ic);                          (* Program line of class definition *)
        let a_class = input_line ic in 
        fprintf oc "%s\n" a_class;
        let inheritable = input_line ic in 
        fprintf oc "%s\n" inheritable;
        let () = if String.equal inheritable "inherits" then 
            let () = fprintf oc "%s\n" (input_line ic) in
            fprintf oc "%s\n" (input_line ic)
        in let feature_length = input_line ic |> int_of_string in 
        fprintf oc "%i\n" feature_length;
        print_features ic oc (Hashtbl.find classes a_class) feature_length;
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
    | (class_name, method_name, a_method) :: tail -> 
        let () = if List.mem class_name ["Bool"; "IO"; "Int"; "Object"; "String"] then
            let () = fprintf oc "%s\n" method_name in 
            let () = fprintf oc "%i\n" (List.length a_method.f_parameters) in
            let () = match method_name with                                     (* Too late now to store the formal names *)
            | "concat"     -> fprintf oc "s\n"
            | "out_int"    -> fprintf oc "x\n"
            | "out_string" -> fprintf oc "x\n"
            | "substr"     -> fprintf oc "i\nl\n" 
            | str -> ()
            in fprintf oc "%s\n0\n%s\ninternal\n%s.%s\n" class_name a_method.info.obj_type class_name method_name
        else
            let () = seek_in ic a_method.info.ic_pointer in
            let () = fprintf oc "%s\n" (input_line ic) in                       (* Method name *)
            let () = fprintf oc "%i\n" (input_line ic |> int_of_string) in      (* Prints # of parameters *)
            let () = for i = 1 to List.length a_method.f_parameters do          (* Prints formal parameter identifier *)
                let _ = input_line ic in                                        (* Formal location   *)
                fprintf oc "%s\n" (input_line ic);                              (* Formal Identifier *)
                let _ = input_line ic, input_line ic in ();                     (* Formal type and location *)
            done in
            let _ = input_line ic, input_line ic in                             (* skips method return type and location *)
            let () = fprintf oc "%s\n" class_name; in
            ignore (print_expressions ic oc a_method.info.t_list)
        in print_methods ic oc tail;;

let rec print_implementation ic oc path =
    match path with
    | [] -> ()
    | head::tail -> 
        fprintf oc "%s\n" head;
        let a_class = Hashtbl.find classes head in
        let method_list = get_method_list head a_class in
        fprintf oc "%i\n" (List.length method_list);
        print_methods ic oc method_list;
        print_implementation ic oc tail;;

let rec print_attributes ic oc attribute_list =
    match attribute_list with
    | [] -> ()
    | a_attr :: tail ->
        let inherits = if List.compare_length_with a_attr.t_list 0 = 0 then "no_initializer" else "initializer" in 
        seek_in ic a_attr.ic_pointer; 
        let identifier         = input_line ic in
        let (* type loc.  *) _ = input_line ic in
        let attribute_type     = input_line ic in
        fprintf oc "%s\n%s\n%s\n" inherits identifier attribute_type;
        if String.equal inherits "initializer" then
            ignore (print_expressions ic oc a_attr.t_list);
        print_attributes ic oc tail;;

let rec print_class ic oc path = 
    match path with
    | [] -> ()
    | head::tail -> 
        fprintf oc "%s\n" head;
        let a_class = Hashtbl.find classes head in
        let attribute_list = get_attribute_list a_class in
        fprintf oc "%i\n" (List.length attribute_list);
        print_attributes ic oc attribute_list;
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
    match input_line ic with                                                                    (* Read in number of classes             *)
    | "0"    -> raise No_class_main;                                                            (* If no class exists, cry no class Main *)
    | length ->                                                                        
        add_classes ic (length |> int_of_string);                                               (* add_classes adds user classes partially to list classes *)
        if Hashtbl.mem classes "Main" |> not then                                               (* Check if class Main exists *)
            raise (No_class_main);
        Hashtbl.iter (has_unknown_parent ic) classes;                                           (* Check all parents for any unknown classes   *)
        let path = topo_sort classes in                                                         (* Create a path for feature type checking     *)
        add_features ic path;
        Hashtbl.iter (add_expressions ic) classes;
        print_type_file ic (String.concat "type" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out) path;;

(* Error handler - Calls main method and handles all errors encountered ------------------------------------------------------------------------------------- *)

let ic = open_in argv.(1) in                    (* Open .cl-ast file *) 
try main ic with
(* Class Exceptions *)
| No_class_main  ->
    printf "ERROR: 0: Type-Check: class Main not found\n";
| No_method_main ->
    printf "ERROR: 0: Type-Check: class Main method main not found\n"
| Method_main_has_parameters ->
    printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n"
| Class_inherits err ->
    printf "ERROR: %s: Type-Check: class %s inherits from %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Class_redefine (location, name) ->
    printf "ERROR: %s: Type-Check: class %s redefined\n" location name
| Class_self_type err     ->
    printf "ERROR: %s: Type-Check: class named SELF_TYPE\n" err
| Class_unknown  err ->
    printf "ERROR: %s: Type-Check: class %s inherits from unknown class %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Cycle err ->       
    printf "ERROR: 0: Type-Check: inheritance cycle: %s" err;
    print_cycle err (Hashtbl.find classes err).parent;
    printf "\n"
    
(* Feature Exceptions *)
| Attribute_redefine err->
    printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2);
| Attribute_self err    ->
    printf "ERROR: %s: Type-Check: class %s has an attribute named self\n"
        (List.hd err) (List.nth err 1)
| Attribute_unknown err ->
    printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)
| Method_redefine err   ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_formal_duplicate err    ->
    printf "ERROR: %s: Type-Check: class %s has method test with duplicate formal parameter named %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_formal_length err       ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes number of formals\n" 
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_formal_redefine err     ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes type of formal %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)
| Method_formal_self err        ->
    printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter named self\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Method_return_redefine err    ->
    printf "ERROR: %s: Type-Check: class %s redefines method %s and changes return type (from %s to %s)\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3) (List.nth err 4);
| Method_unknown_formal_type err ->
    printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter of unknown type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)
| Method_unknown_return_type err ->
    printf "ERROR: %s: Type-Check: class %s has method %s with unknown return type %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3)

(* Expression Exceptions *)
| Arithmetic_type err ->
    printf "ERROR: %s: Type-Check: arithmetic on %s %s instead of Ints\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Assign_self err ->
    printf "ERROR: %s: Type-Check: cannot assign to self\n" err
| Assign_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in assignment\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Attribute_nonconform err ->
    printf "ERROR: %s: Type-Check: %s does not conform to %s in initialized attribute\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Case_self err  ->
    printf "ERROR: %s: Type-Check: binding self in a case expression is not allowed\n" err
| Case_self_type err ->
    printf "ERROR: %s: Type-Check: using SELF_TYPE as a case branch type is not allowed\n" err
| Case_type_duplicate err ->
    printf "ERROR: %s: Type-Check: case branch type %s is bound twice\n"
        (List.hd err) (List.nth err 1)
| Dispatch_unknown err ->
    printf "ERROR: %s: Type-Check: unknown method %s in dispatch on %s\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Dispatch_parameter_mismatch err ->
    printf "ERROR: %i: Type-Check: wrong number of actual arguments (%i vs. %i)\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| Dispatch_parameter_nonconform err ->
    printf "ERROR: %s: Type-Check: argument #%s type %s does not conform to formal type %s\n"
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
    printf "ERROR: %s: Type-Check: initializer type %s does not conform to type %s\n"
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
| Wrong_type err ->
    printf "ERROR: %s: Type-Check: %s has type %s instead of Bool\n"
        (List.hd err) (List.nth err 1) (List.nth err 2)
| _ -> ();