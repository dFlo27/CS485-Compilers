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

(* Type a_class, referred to as class_info in the functions, contains all of the important info a class contains.
    The Hashtbl classes will contain the pairs (string, a_class), where string is the class name and a_class will be the class info *)
type a_class = {
    parent          : string;
    attribute_env   : (string, string) Hashtbl.t;
    method_env      : (string, string * string list) Hashtbl.t;
    ic_pointers     : int list;
};;

let empty_a_hash = Hashtbl.create 0;;       (* This is for classes with no attributes. *)
let empty_m_hash = Hashtbl.create 0;;       (* This is for classes with no methods.  *)
let m_obj_hash   = Hashtbl.create 0;;       (* This is for Object *)
let () = Hashtbl.add m_obj_hash "abort"     ("Object"   , []);;
let () = Hashtbl.add m_obj_hash "type_name" ("Object"   , []);;
let () = Hashtbl.add m_obj_hash "copy"      ("SELF_TYPE", []);;
let m_io_hash = Hashtbl.create 0;;          (* This is for IO *)
let () = Hashtbl.add m_io_hash "out_string" ("SELF_TYPE", ["String"]);;
let () = Hashtbl.add m_io_hash "out_int"    ("SELF_TYPE", ["Int";  ]);;
let () = Hashtbl.add m_io_hash "in_string"  ("String", []);;
let () = Hashtbl.add m_io_hash "in_int"     ("Int"   , []);;
let m_string_hash = Hashtbl.create 0;;      (* This is for String *)
let () = Hashtbl.add m_string_hash "length" ("Int",    []);;
let () = Hashtbl.add m_string_hash "concat" ("String", ["String"    ]);;
let () = Hashtbl.add m_string_hash "substr" ("String", ["Int"; "Int"]);;

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


let check_parents ic class_name class_info =
    if class_name = "Object" then ()
    else match Hashtbl.mem classes class_info.parent with
    | true  -> ()
    | false -> 
        seek_in ic (List.hd class_info.ic_pointers);     
        let _ = input_line ic, input_line ic, input_line ic in           
        raise (Class_unknown [input_line ic; class_name; class_info.parent]);;

let create_path class_name class_info path = [class_info.parent; class_name] :: path;;


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

let rec find_method identifier class_name class_info =
    match Hashtbl.find_opt class_info.method_env identifier with
    | Some m -> m
    | None -> 
        match String.equal "Object" class_name with 
        | true  -> ("", [])
        | false -> find_method identifier class_info.parent (Hashtbl.find classes class_info.parent);;

let handle_main class_info = 
    match find_method "main" "Main" class_info with
    | ("", []) -> raise (No_method_main)
    | (_,  []) -> ()
    | _ -> raise (Method_main_has_parameters);;

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
        let (* identifier loc.   *) _ = input_line ic in
        let identifier                = input_line ic in
        let (* type loc.         *) _ = input_line ic in
        let attribute_type            = input_line ic in
        if String.equal inherits "attribute_init" then 
            fprintf oc "initializer\n"
        else fprintf oc "no_initializer\n";
        fprintf oc "%s\n%s\n" identifier attribute_type;
        if String.equal inherits "attribute_init" then
            print_body ic oc;
        print_class_attributes ic oc (length - 1);;

let rec print_cycle origin curr =
    if String.equal origin curr |> not then
        let () = printf " %s" curr in
        print_cycle origin (Hashtbl.find classes curr).parent;;

let rec print_all_attributes ic oc class_name class_info =
    match class_name with
    | "Bool" | "Int" | "IO" | "Object" | "String" -> ();
    | _ ->
        print_all_attributes ic oc class_info.parent (Hashtbl.find classes class_info.parent);
        seek_in ic (List.nth class_info.ic_pointers 1);
        let (* feature length *) _ = input_line ic in
        print_class_attributes ic oc (Hashtbl.length class_info.attribute_env);;


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

let rec new_expression ic =                                                             (* Reads expressions for now *)
    let program_line = input_line ic in                                                 (* Nearly all expressions have a program line in the ast tree*)
    if program_line = "let_binding_init" || program_line = "let_binding_no_init" then   (* Let bindings annoyingly don't have a program line... *)
        let _ = () in match program_line with                                           (* Appeasing the Ocaml Gods with randome let statements *)
        | "let_binding_init" ->    
            let (* ident. loc, ident. *)_ = input_line ic, input_line ic in
            let (* type loc, type *) _ = input_line ic, input_line ic in
                new_expression ic;
        | "let_binding_no_init" ->
            let (* ident. loc, ident. *)_ = input_line ic, input_line ic in 
            let (* type loc, type *) _ = input_line ic, input_line ic in (); [];
        | str -> raise (Failure "Failed: new_expression, let_binding\n");
    else match input_line ic with
    | "assign" -> 
        let (* ident. loc *) _ = input_line ic in 
        let (* ident. *)     _ = input_line ic in
            (* Initializer *) new_expression ic;
    | "case"   ->
        let (* ident. *) _ = new_expression ic in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let (* ident. loc *) _ = input_line ic in
            let (* ident. *)     _ = input_line ic in
            let (* type loc *)   _ = input_line ic in
            let (* type *)       _ = input_line ic in
            let (* case body *)  _ = new_expression ic in (); 
        done; []
    | "dynamic_dispatch" ->
        let (* ident. *) _ = new_expression ic in let (* method & loc *) _ = input_line ic, input_line ic in 
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ();
        done; [];
    | "self_dispatch" ->  
        let (* method & loc *) _ = input_line ic, input_line ic in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ();
        done; [];
    | "static_dispatch" -> 
        let (* ident *) _       = new_expression ic in 
        let (* superclass *) _  = input_line ic, input_line ic in 
        let (* method & loc*) _ = input_line ic, input_line ic in
        for i = 1 to input_line ic |> int_of_string (* # of arguments *) do
            let _ = new_expression ic in ();
        done; [];
    | "block"   -> 
        for i = 1 to input_line ic |> int_of_string (* # of expr. in block *) do
            let _ = new_expression ic in ();
        done; [];
    | "identifier" -> let (* ident. loc, ident. *)_ = input_line ic, input_line ic in [];
    | "if" -> 
        let (* cond. *) _ = new_expression ic in 
        let (* then  *) _ = new_expression ic in 
        let (* else  *) _ = new_expression ic in []; 
    | "integer" -> let (* integer itself *) _ =  input_line ic in [];
    | "isvoid" -> new_expression ic;
    | "let" -> 
        for i = 1 to input_line ic |> int_of_string (* # of bindings *) do
            let _ = new_expression ic in ();
        done; 
        (* Body of let *) new_expression ic;
    | "negate" -> new_expression ic;
    | "new" -> let (* class & loc *) _ = input_line ic, input_line ic in [];
    | "not" -> new_expression ic;
    | "plus" | "minus" | "times" | "divide" | "eq" | "lt" | "le" -> 
        let _ = (* Left - Side *) new_expression ic, (* Right - Side *) new_expression ic in [];
    | "string" -> let (* string *) _ = input_line ic in [];
    | "true" -> [];
    | "false" -> [];
    | "while" -> let (* cond. *) _ = new_expression ic in (* body *) new_expression ic;
    | str -> raise (Failure "new_expression failed");;

let rec compare_formals ic previous original length =
    match length with
    | 0 -> 
        let type_location = input_line ic in let (* type *) _ = input_line ic in
        if String.equal (fst previous) (fst original) |> not then
            raise (Method_return_redefine [type_location; fst original; fst previous]);
    | _ ->
        let (* ident._location *) _ = input_line ic in let identifier   = input_line ic in
        let formal_location         = input_line ic in let (* type *) _ = input_line ic in
        if String.equal (snd previous |> List.hd) (snd original |> List.hd) |> not then 
            raise (Method_formal_redefine [formal_location; identifier])
        else compare_formals ic (fst previous, snd previous |> List.tl) (fst original, snd original |> List.tl) (length - 1);;

let rec new_formals ic names types length = 
    match length with
    | 0 -> 
        let type_location = input_line ic in let return_type   = input_line ic in 
        if String.equal return_type "SELF_TYPE" |> not && Hashtbl.mem classes return_type |> not then
            raise (Method_unknown_return_type [type_location; return_type]);
        (return_type, types);
    | _ -> 
        let identifier_location = input_line ic in let identifier  = input_line ic in
        let formal_location     = input_line ic in let formal_type = input_line ic in
        if String.equal identifier "self"          then raise (Method_formal_self         [identifier_location]);
        if List.mem     identifier names           then raise (Method_formal_duplicate    [identifier_location; identifier]);
        if Hashtbl.mem  classes formal_type |> not then raise (Method_unknown_formal_type [formal_location; formal_type])
        else new_formals ic (identifier :: names) (types @ [formal_type]) (length - 1);;

let rec read_features ic class_name class_info length = 
    match length with
    | 0 -> ();
    | _ -> 
        let feature             = input_line ic in
        let identifier_location = input_line ic in
        let identifier          = input_line ic in
        match feature with
        | "attribute_init" | "attribute_no_init" ->
            if String.equal identifier "self" then 
                raise (Attribute_self [identifier_location; class_name]);
            if attribute_has_ancestor identifier class_name class_info then
                raise (Attribute_redefine [identifier_location; class_name; identifier]);
            let type_location  = input_line ic in
            let type_attribute = input_line ic in
            if String.equal type_attribute "SELF_TYPE" |> not && Hashtbl.mem classes type_attribute |> not then
                raise (Attribute_unknown [type_location; class_name; identifier; type_attribute]);
            Hashtbl.add class_info.attribute_env identifier type_attribute;
            let () = if feature = "attribute_init" then
                let _ = expressions_skip ic in ();
            in read_features ic class_name class_info (length - 1);
        | "method" -> 
            if Hashtbl.mem class_info.method_env identifier then
                raise (Method_redefine [identifier_location; class_name; identifier]);
            let formals_length  = input_line ic |> int_of_string in
            let formals_location = pos_in ic in
            let prev_formals = find_method identifier class_info.parent (Hashtbl.find classes class_info.parent) in
            let curr_formals = try (new_formals ic [] [] formals_length) with
            | Method_formal_duplicate    err -> raise (Method_formal_duplicate    [List.hd err; class_name; List.nth err 1]            );
            | Method_formal_self         err -> raise (Method_formal_self         [List.hd err; class_name; identifier    ]            );
            | Method_unknown_formal_type err -> raise (Method_unknown_formal_type [List.hd err; class_name; identifier; List.nth err 1]); 
            | Method_unknown_return_type err -> raise (Method_unknown_return_type [List.hd err; class_name; identifier; List.nth err 1]); 
            in let () = if String.equal (fst prev_formals) "" then
                Hashtbl.add class_info.method_env identifier curr_formals
            else let () = 
                seek_in ic formals_location;
                if snd prev_formals |> List.length != formals_length then
                    raise (Method_formal_length    [identifier_location; class_name; identifier]);
                try compare_formals ic prev_formals curr_formals formals_length with 
                | Method_formal_redefine err -> raise (Method_formal_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
                | Method_return_redefine err -> raise (Method_return_redefine  ([List.hd err; class_name; identifier] @ List.tl err));
                in Hashtbl.add class_info.method_env identifier prev_formals;
            in let _ = expressions_skip ic in
                read_features ic class_name class_info (length - 1);
        | _ -> raise (Failure "read_features failed");;

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
            ic_pointers   = [class_pointer; feature_pointer]
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
        Hashtbl.iter (check_parents ic) classes;                                                     (* Check all parents for any unknown classes   *)
        let path = List.tl (Hashtbl.fold create_path classes [] |> topo_sort) in                     (* Create a path for feature type checking     *)
        add_features ic path;                                                                        (* List.tl removes Object's parent from path   *)
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