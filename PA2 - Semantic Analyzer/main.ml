open Printf;;
open Hashtbl;;
open Sys;;

(* All type check errors will raise one of these exceptions *)

exception No_class_main;;
exception No_method_main;;
exception Method_main_has_parameters;;
exception Cycle                      of string;;
exception Class_redefine             of string * string;;
exception Class_self_type            of string;;
exception Class_inherits             of string list;;
exception Class_unknown              of string list;;
exception Attribute_redefine         of string list;;
exception Attribute_self             of string list;;
exception Attribute_unknown          of string list;;
exception Method_redefine            of string list;;
exception Method_formal_duplicate    of string list;;
exception Method_formal_length       of string list;;
exception Method_formal_redefine     of string list;;
exception Method_formal_self         of string list;;
exception Method_return_redefine     of string list;;
exception Method_unknown_formal_type of string list;;
exception Method_unknown_return_type of string list;;

(* Data Structures - The record is to store a class's info and the hash tables are to store environments and classes ---------------------------------------- *)

type object_id = {
    a_type : (string * string);
    t_list : string list;
};;

type a_method = {
    f_parameters   : (object_id) list;
    info           : object_id;
};; 

(* Object_id (Attributes, Formals, Let Bindings, etc.) Functions *)
let new_object_id t = {a_type = (t, t); t_list = []};;          (* Given a type, returns a new object_id *)
let get_static  id = fst id.a_type;;                            (* Returns static type of object_id *)
let get_dynamic id = snd id.a_type;;                            (* Returns dynamic type of object_id *)

(* A_Method Functions *)
(* Given a type and pair list containing identifiers and type for each formal, returns a new a_method *)
let new_a_method t args = {info = new_object_id t; f_parameters = List.map new_object_id args};;
let get_m_static  id = fst id.info.a_type;;                       (* Returns static type of a_method *)
let get_m_dynamic id = snd id.info.a_type;;                       (* Returns dynamic type of a_method *)

(* Type a_class, referred to as class_info in the functions, contains all of the important info a class contains.
    The Hashtbl classes will contain the pairs (string, a_class), where string is the class name and a_class will be the class info *)
type a_class = {
    parent        : string;
    attribute_env : (string, object_id) Hashtbl.t;
    method_env    : (string, a_method ) Hashtbl.t;
    ic_pointers   : int list;
};;

(* Base Classes - Stores all the info about COOL's standard classes. Contains empty hashes for later use too ------------------------------------------------ *)

let empty_method = new_a_method "" [];;
let empty_a_hash = Hashtbl.create 0;;       (* This is for classes with no attributes. *)
let empty_m_hash = Hashtbl.create 0;;       (* This is for classes with no methods.  *)
let m_obj_hash   = Hashtbl.create 0;;       (* This is for Object *)
let () = Hashtbl.add m_obj_hash "abort"     (new_a_method "Object"    []);;
let () = Hashtbl.add m_obj_hash "type_name" (new_a_method "String"    []);;
let () = Hashtbl.add m_obj_hash "copy"      (new_a_method "SELF_TYPE" []);;
let m_io_hash = Hashtbl.create 0;;          (* This is for IO *)
let () = Hashtbl.add m_io_hash "out_string" (new_a_method "SELF_TYPE" ["String"]);;
let () = Hashtbl.add m_io_hash "out_int"    (new_a_method "SELF_TYPE" ["Int"]   );;
let () = Hashtbl.add m_io_hash "in_string"  (new_a_method "String" []);;
let () = Hashtbl.add m_io_hash "in_int"     (new_a_method "Int"    []);;
let m_string_hash = Hashtbl.create 0;;      (* This is for String *)
let () = Hashtbl.add m_string_hash "length" (new_a_method "Int"    []);;
let () = Hashtbl.add m_string_hash "concat" (new_a_method "String" ["String"]);;
let () = Hashtbl.add m_string_hash "substr" (new_a_method "String" ["Int"; "Int"]);;
let classes    = Hashtbl.create 50;;        (* Hash containing all classes *)
let () = Hashtbl.add classes "Object" {parent = "";       ic_pointers = []; attribute_env = empty_a_hash; method_env = m_obj_hash   };;
let () = Hashtbl.add classes "IO"     {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_io_hash    };;
let () = Hashtbl.add classes "Int"    {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = empty_m_hash };;
let () = Hashtbl.add classes "String" {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = m_string_hash};;
let () = Hashtbl.add classes "Bool"   {parent = "Object"; ic_pointers = []; attribute_env = empty_a_hash; method_env = empty_m_hash };;

(* Helper Functions ----------------------------------------------------------------------------------------------------------------------------------------- *)

(* Checks whether an attribute has been defined before in one of the class's ancestors. Return boolean of its existence *)
let rec attribute_has_ancestor identifier child_name child_info =
    match child_name with
    | "Object" -> false
    | _        -> 
    if Hashtbl.mem child_info.attribute_env identifier then 
        true
    else attribute_has_ancestor identifier child_info.parent (Hashtbl.find classes child_info.parent);;

(* Returns the # of attributes a given class has. The way attributes are stored is only the class who defined the attribute actually contains it.
    Every child class will need to recursively search its parents to get all the attributes it has *)
let rec attribute_length class_name class_info acc =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> acc;
    | _ -> attribute_length class_info.parent (Hashtbl.find classes class_info.parent) (acc + Hashtbl.length class_info.attribute_env);;

(* Checks if class's parent is defined in the program. If it's not, raise Class_unknown error *)
let has_unknown_parent ic class_name class_info =
    if class_name != "Object" && Hashtbl.mem classes class_info.parent |> not then
        let () = seek_in ic (List.hd class_info.ic_pointers) in
        let _  = input_line ic, input_line ic, input_line ic  in           
        raise (Class_unknown [input_line ic; class_name; class_info.parent]);;

(* Adds a parent to child edge to a list that will be sorted by topological sort *)
let create_path class_name class_info path = [class_info.parent; class_name] :: path;;

(* Least Upper Bound Function - Computes the common ancestor class of two classes. Will always return a class hopefully. ------------------------------------ *)
let rec find_common_ancestor tree_1 tree_2 prev_class = 
    if String.equal (List.hd tree_1) (List.hd tree_2) |> not then
        prev_class
    else find_common_ancestor (List.tl tree_1) (List.tl tree_2) (List.hd tree_1);;

let rec get_ancestor_tree a_class =
    if String.equal a_class "Object" |> not then 
        let c_info = Hashtbl.find classes a_class in
        c_info.parent :: get_ancestor_tree c_info.parent
    else []

let lub class_1 class_2 = 
    if class_1 = class_2 then class_1
    else 
        let class_1_ancestors = get_ancestor_tree class_1 |> List.rev in
        let class_2_ancestors = get_ancestor_tree class_2 |> List.rev in
        find_common_ancestor class_1_ancestors class_2_ancestors "Object";;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let rec conforms_to p_type c_type = 
    if c_type != p_type && c_type != "Object" then
        conforms_to p_type (Hashtbl.find classes c_type).parent
    else if c_type = "Object" then false
    else true;; 


(* Provides a way to skip expressions
    There are two cases:
        1. If the 3rd line scanned is inherits/no_inherits -> Move ic to 'start' (ic_pointer1) and return
        2. If the 3rd line scanned isn't inherits          -> Move ic after 'start' (ic_pointer2) and call feature_skip
    Remember: Classes are stored in the AST as: class_location, class_name, inherits, ... *)
let rec expressions_skip ic =
    let ic_point1 = pos_in ic in
    try let input = input_line ic in
    if input = "method" || input = "attribute_init" || input = "attribute_no_init" then
        seek_in ic ic_point1
    else 
        let ic_point2 = pos_in ic in
        let _     = input_line ic in
        let input = input_line ic in
        if  input = "inherits" || input = "no_inherits" then
            seek_in ic ic_point1
        else let () = seek_in ic ic_point2 in
            expressions_skip ic;
    with End_of_file -> ();;

let rec features_skip ic length =
    match length with 
    | 0 -> ();
    | _  -> let _ = input_line ic in
            expressions_skip ic; 
            features_skip ic (length - 1);;

let rec find_method identifier class_info =
    try Hashtbl.find class_info.method_env identifier
    with Not_found -> if class_info.parent != ""
        then find_method identifier (Hashtbl.find classes class_info.parent)
        else empty_method;;

let handle_main class_info = 
    let m = find_method "Main" class_info in
    if m = empty_method then
        raise (No_method_main);
    if m.f_parameters != [] then
        raise (Method_main_has_parameters);;

let rec print_body ic oc =
    let ic_pointer = pos_in ic in
    try let line   = input_line ic in
    match line with
    | "method" | "attribute_init" | "attribute_no_init" -> 
        seek_in ic ic_pointer; ();
    | _ ->  
        let () = fprintf oc "%s\n" line in
            print_body ic oc;
    with End_of_file -> ();;

let rec print_class_attributes ic oc length =
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
            print_body ic oc;
        print_class_attributes ic oc (length - 1);;

let rec print_all_attributes ic oc class_name class_info =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> ();
    | _ ->
        print_all_attributes ic oc class_info.parent (Hashtbl.find classes class_info.parent);
        seek_in ic (List.nth class_info.ic_pointers 1);
        let (* feature length *) _ = input_line ic in
        print_class_attributes ic oc (Hashtbl.length class_info.attribute_env);;

let rec print_cycle start curr =
    if String.equal start curr |> not then
        let () = printf " %s" curr in
        print_cycle start (Hashtbl.find classes curr).parent;;


(*  Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

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
let topo_sort edges =
    let nodes = List.flatten edges |> List.sort_uniq String.compare in                  (* Effectively a set containing all nodes in edges *)
        let s = List.fast_sort String.compare (List.filter (is_source edges) nodes) in  (* List s stores all sources in nodes              *)
            kahn_algorithm s edges;;                                                    (* Function call to Kahn's algorithm               *)

(* AST Tree Parser Functions -------------------------------------------------------------------------------------------------------------------------------- *)

let rec read_expression ic class_info =                                                 (* Reads expressions for now *)
    let (* program_line *) _ = input_line ic in                                         
    match input_line ic with
    | "assign" -> 
        let (* ident. loc *) _ = input_line ic in 
        let identifier = input_line ic in
        let ident_info = Hashtbl.find_opt class_info.attribute_env identifier in
        if ident_info = None then 
            raise (Failure "Unknown identifier in assign\n");
        let ident_info = Option.get ident_info in let init_type = read_expression ic class_info in
        if conforms_to (fst ident_info.a_type) (List.hd init_type) |> not then
            raise (Failure "Identifier does not conform to identifier type in assign\n");
        Hashtbl.add class_info.attribute_env identifier {a_type = (get_static ident_info, List.hd init_type); t_list = []};
        List.hd init_type :: init_type
    | "block" -> 
        let type_list = ref [] in
        for i = 1 to (input_line ic |> int_of_string)  - 1 do
            type_list := !type_list @ read_expression ic class_info;
        done;
        let last_expr = read_expression ic class_info in
        List.hd last_expr :: (!type_list @ last_expr);
    | "case"  ->
        let ident_list = read_expression ic class_info in
        let case_list = ref [] in let case_type = ref "" in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let (* ident. loc *) _ = input_line ic in let identifier = input_line ic in
            if String.equal identifier "self" then
                raise (Failure "Case expression failed, ident is self\n");
            let (* type loc *)   _ = input_line ic in let ident_type = input_line ic in
            if Hashtbl.mem classes ident_type |> not then
                raise (Failure "Case expression failed, type doesn't exist\n");
            let case_body = read_expression ic class_info in 
            case_list := !case_list @ case_body;
            if i = 1 then
                case_type := List.hd case_body
            else
                case_type := lub (!case_type) (List.hd case_body); 
        done;
        !case_type :: ident_list @ !case_list;
    | "dynamic_dispatch" ->
        let ident_list = read_expression ic class_info in 
        let (* method_loc *) _ = input_line ic in 
        let method_name  = input_line ic in 
        let method_class = Hashtbl.find_opt classes (List.hd ident_list) in
        if  method_class = None then
            raise (Failure "Dynamic dispatch fail, unknown class\n");
        let method_info = find_method method_name (Option.get method_class) in 
        if method_info = empty_method then
            raise (Failure "Dynamic dispatch fail, unknown method\n");
        let type_list = ref [] in let arguments = ref [] in
        let method_type = get_m_static method_info in
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do 
            let arg = read_expression ic class_info in
            if conforms_to method_type (List.hd arg) |> not then 
                raise (Failure "Dynamic dispatch fail, argument does not conform formal static type\n");
            arguments := !arguments @ arg;
        done; 
        let type_list = !type_list @ !arguments @ (read_expression ic class_info) in
        method_type :: type_list @ ident_list;
    | "self_dispatch" ->  
        let (* method loc *) _ = input_line ic in let method_name = input_line ic in 
        let method_info = Hashtbl.find_opt class_info.method_env method_name in
        if method_info = None then 
            raise (Failure "Self dispatch fail, unknown method\n");
        let type_list = ref [] in let arguments = ref [] in
        let method_type = get_m_static (Option.get method_info) in
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do 
            let arg = read_expression ic class_info in
            if conforms_to method_type (List.hd arg) |> not then 
                raise (Failure "Self dispatch fail, argument does not conform formal static type\n");
            arguments := !arguments @ arg;
        done; 
        let type_list = !type_list @ !arguments @ (read_expression ic class_info) in
        method_type :: type_list;
    | "static_dispatch" -> 
        let ident_list = read_expression ic class_info in 
        let (* parent_loc *) _ = input_line ic in let parent = input_line ic in 
        let method_name  = input_line ic in 
        let method_class = Hashtbl.find_opt classes (List.hd ident_list) in
        if  method_class = None then
            raise (Failure "Static dispatch fail, unknown class\n");
        if conforms_to parent (List.hd ident_list) |> not then
            raise (Failure "Parent class does not conform to ident\n"); 
        let method_info = find_method method_name (Option.get method_class) in 
        if method_info = empty_method then
            raise (Failure "Static dispatch fail, unknown method\n");
        let type_list = ref [] in let arguments = ref [] in
        let method_type = get_m_static method_info in
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do 
            let arg = read_expression ic class_info in
            if conforms_to method_type (List.hd arg) |> not then 
                raise (Failure "Static dispatch fail, argument does not conform formal static type\n");
            arguments := !arguments @ arg;
        done; 
        let type_list = !type_list @ !arguments @ (read_expression ic class_info) in
        method_type :: type_list @ ident_list;
    | "eq" | "lt" | "le" -> 
        let left_type = read_expression ic class_info in let right_type = read_expression ic class_info in
        if String.equal (List.hd left_type) (List.hd right_type) |> not then
            raise (Failure "Equality expression failed, types are not equal\n");
        "Bool" :: left_type @ right_type;
    | "identifier" -> 
        let (* ident_loc *) _ = input_line ic in let identifier = input_line ic in 
        let object_id = Hashtbl.find_opt class_info.attribute_env identifier in
        if  object_id = None then
            raise (Failure "identifier not found in class_info attributes\n")
        else [Option.get object_id |> get_static];
    | "if" -> 
        let cond_type = read_expression ic class_info in 
        if String.equal (List.hd cond_type) "Bool" then 
            raise (Failure "Nonconforming expression for while loop\n");
        let then_type = read_expression ic class_info in 
        let else_type = read_expression ic class_info in 
        lub (List.hd then_type) (List.hd else_type) :: (cond_type @ then_type @ else_type); 
    | "integer" -> let (* integer *) _ =  input_line ic in ["Int"];
    | "isvoid"  -> "Bool" :: read_expression ic class_info;
    | "let" -> 
        let type_list = ref [] in 
        let arguments = ref [] in 
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do 
            let binding   = input_line ic in 
            let (* ident_loc *) _ = input_line ic in let identifier = input_line ic in
            let (* type_loc  *) _ = input_line ic in let bind_type  = input_line ic in
            if Hashtbl.mem classes bind_type |> not then
                raise (Failure "Unknown class in let expression\n");
            Hashtbl.add class_info.attribute_env identifier (new_object_id bind_type);
            arguments := identifier :: !arguments;
            if String.equal binding "let_binding_init" then
                let init_type = read_expression ic class_info in
                if conforms_to (List.hd init_type) bind_type |> not then
                    raise (Failure "Initialization does not conform to static type")
                else type_list := !type_list @ init_type;
        done; 
        let type_list = !type_list @ (read_expression ic class_info) in
        List.iter (Hashtbl.remove class_info.attribute_env) !arguments;
        type_list;
    | "negate" -> "Int" :: read_expression ic class_info;
    | "new" -> 
        let (* class loc. *) _ = input_line ic in let new_class = input_line ic in 
        if Hashtbl.mem classes new_class |> not then 
            raise (Failure "Unknown class in new expression\n");
        [new_class];         
    | "not" -> 
        let type_list = read_expression ic class_info in
        if String.equal (List.hd type_list) "Bool" |> not then 
            raise (Failure "Expression Not doesn't have type Bool\n");
        "Bool" :: type_list;
    | "plus" | "minus" | "times" | "divide" -> 
        let left_type = read_expression ic class_info in let right_type = read_expression ic class_info in 
        if String.equal (List.hd left_type) "Int" && String.equal (List.hd right_type) "Int" then 
            "Int" :: (left_type @ right_type)
        else raise (Failure "Nonconforming plus/minus/times/divide\n");
    | "string" -> let (* string *) _ = input_line ic in ["String"];
    | "true" | "false" -> ["Bool"];
    | "while"  -> 
        let cond_type = read_expression ic class_info in 
        if String.equal (List.hd cond_type) "Bool" |> not then
            raise (Failure "Nonconforming condition expr for while loop\n");
        let body_type = read_expression ic class_info in "Object" :: cond_type @ body_type;
    | str -> raise (Failure "read_expression failed\n");;

let rec initialize_features ic class_info feature_length =
    if feature_length != 0 then
        let feature    = input_line ic in
        let identifier = input_line ic in let (* i_location *) _ = input_line ic in
        let () = match feature with
        | "attribute_no_init" -> expressions_skip ic;
        | "attribute_init"    ->
            let attribute = Hashtbl.find class_info.attribute_env identifier in
            let type_list = read_expression ic class_info in
            if conforms_to (get_static attribute) (List.hd type_list) then
                raise (Failure "(*Here lies nonconforming attribute error*)");
            Hashtbl.replace class_info.attribute_env identifier {a_type = (get_static attribute, List.hd type_list); t_list = type_list};
        | "method" -> ()
        | _ -> raise (Failure "initialize_features failed")
    in initialize_features ic class_info feature_length;;

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
    | [] -> raise (Failure "compare_formals_body is bad\n")
let compare_formals loc_and_ident prev_method new_method =
    compare_formals_body loc_and_ident (get_m_static prev_method, prev_method.f_parameters) (get_m_static new_method, prev_method.f_parameters);;

let rec new_formals_body ic formal_list extra_info length = 
    match length with
    | 0 -> 
        let type_location = input_line ic in let return_type = input_line ic in 
        if String.equal return_type "SELF_TYPE" |> not && Hashtbl.mem classes return_type |> not then
            raise (Method_unknown_return_type [type_location; return_type]);
        new_a_method return_type (List.rev formal_list), (("", type_location) :: extra_info |> List.rev);
    | _ -> 
        let identifier_location = input_line ic in let identifier  = input_line ic in
        let type_location       = input_line ic in let formal_type = input_line ic in
        if String.equal identifier "self" 
            then raise (Method_formal_self [identifier_location]);
        if List.exists (fun ele -> String.equal (fst ele) identifier) extra_info 
            then raise (Method_formal_duplicate [identifier_location; identifier]);
        if Hashtbl.mem  classes formal_type |> not then 
            raise (Method_unknown_formal_type [type_location; formal_type])
        else new_formals_body ic (formal_type :: formal_list) ((identifier, type_location) :: extra_info) (length - 1);;
let new_formals ic length = new_formals_body ic [] [] length;;

let rec read_features ic class_name class_info length = 
    match length with
    | 0 -> ();
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
            in read_features ic class_name class_info (length - 1);
        | "method" -> 
            if Hashtbl.mem class_info.method_env identifier then
                raise (Method_redefine [ident_location; class_name; identifier]);
            let formals_length  = input_line ic |> int_of_string in
            let prev_method = find_method identifier (Hashtbl.find classes class_info.parent) in
            let method_info = try new_formals ic formals_length with
            | Method_formal_duplicate    err -> raise (Method_formal_duplicate    [List.hd err; class_name; List.nth err 1]            );
            | Method_formal_self         err -> raise (Method_formal_self         [List.hd err; class_name; identifier    ]            );
            | Method_unknown_formal_type err -> raise (Method_unknown_formal_type [List.hd err; class_name; identifier; List.nth err 1]); 
            | Method_unknown_return_type err -> raise (Method_unknown_return_type [List.hd err; class_name; identifier; List.nth err 1]); 
            in Hashtbl.add class_info.method_env identifier (fst method_info);
            let () = if prev_method != empty_method then
                if prev_method.f_parameters |> List.length != formals_length then
                    raise (Method_formal_length [ident_location; class_name; identifier])
                else try compare_formals (snd method_info) prev_method (fst method_info) with 
                | Method_formal_redefine err -> raise (Method_formal_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
                | Method_return_redefine err -> raise (Method_return_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
            in let () = expressions_skip ic in
            read_features ic class_name class_info (length - 1);
        | _ -> raise (Failure "read_features failed");;

let add_expressions ic class_name class_info =
    seek_in ic (List.nth class_info.ic_pointers 1);
    let feature_length = input_line ic |> int_of_string in
    try initialize_features ic class_info feature_length with | _ -> ();;

let rec add_features ic path =
    match path with
    | [] -> ();
    | class_name::tail ->
        let class_info = Hashtbl.find classes class_name in
        if List.compare_length_with class_info.ic_pointers 0 != 0 then
            let () = seek_in ic (List.nth class_info.ic_pointers 1) in 
            read_features ic class_name class_info (input_line ic |> int_of_string)
        else (); 
        if String.equal class_name "Main" then handle_main class_info;
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
            raise (Class_redefine (program_line, class_name))
        else let inheritable = (input_line ic = "inherits") in
        let class_inheritance = if inheritable then (input_line ic, input_line ic) else ("Object", "") in
        if inheritable && List.mem (fst class_inheritance) ["Bool"; "Int"; "String"] then 
            raise (Class_inherits [snd class_inheritance; class_name; fst class_inheritance]);
        let feature_pointer = pos_in ic in
        let feature_length = input_line ic |> int_of_string in 
        features_skip ic feature_length;
        let class_info = {
            parent        = fst class_inheritance;
            attribute_env = Hashtbl.create 50;
            method_env    = Hashtbl.create 50;
            ic_pointers   = [class_pointer; feature_pointer];
        } in Hashtbl.add classes class_name class_info;
        add_classes ic (length - 1);;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let rec print_map ic oc path = 
    match path with
    | [] -> close_out oc;
    | head::tail -> 
        fprintf oc "%s\n" head;
        let class_info = Hashtbl.find classes head in
        fprintf oc "%i\n" (attribute_length head class_info 0);
        if List.compare_length_with class_info.ic_pointers 0 = 0 |> not then 
            print_all_attributes ic oc head class_info;
        print_map ic oc tail;;

(* 'Main' Method of the program. Calls all functions and returns either an error or a .cl-ast file ---------------------------------------------------------- *)
let main ic =
    match input_line ic with                                                                         (* Handles the empty file case                 *)
    | "0"    -> raise No_class_main;                                                                 (* If no class exists, cry no class Main       *)
    | length ->                                                                          
        add_classes ic (length |> int_of_string);                                                    (* Add program classes to hash table classes   *)
        if Hashtbl.mem classes "Main" |> not then
            raise (No_class_main);
        Hashtbl.iter (has_unknown_parent ic) classes;                                                (* Check all parents for any unknown classes   *)
        let path = List.tl (Hashtbl.fold create_path classes [] |> topo_sort) in                     (* Create a path for feature type checking     *)
        add_features ic path;                                                                        
        Hashtbl.iter (add_expressions ic) classes;
        let oc = String.concat "type" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 3)); ""] |> open_out in
            fprintf oc "class_map\n%i\n" (Hashtbl.length classes);
            List.fast_sort String.compare path |> print_map ic oc;
            close_in ic;
            close_out oc;;

(* Error handler - Calls main method and handles all errors encountered ------------------------------------------------------------------------------------- *)

let ic = open_in argv.(1) in (* Opens input file. will crash if file does not exist *)      
try main ic with
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
        (List.hd err) (List.nth err 1) (List.nth err 2) (List.nth err 3);;
(*
    1st Pass: Read classes and type check them. We DONT read attributes nor methods because of lack of info from environment and parents.
        Then we should order classes to know which class to safely type check attributes and methods (not expressions) FIRST.
    2nd Pass: Read the attributes and methods of each class in order to build an environment for attributes and methods for EACH class. We can type check for
        attribute or method redefinition.
    3rd Pass: Now read attributes initializations and method bodies. We have all class info to type check this part.
*)