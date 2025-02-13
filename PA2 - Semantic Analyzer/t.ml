open Hashtbl
open Printf
exception IllClass of string
(* Topological Sort - Kahn's Algorithm --------------------------------------------------------------------------------------------------------------------- *)

(* Simple Search Algorithm - O(n) where n is length of lst; Returns answer::tail or [].
   The parameter tf is used to transform an element to string for comparison *)
let rec exists lst str tf = 
    match lst with
    | [] -> []
    | head::tail ->
        match String.equal str (tf head) with
        | true  -> head::tail
        | false -> exists tail str tf;;

(* Checks whether str isn't an incoming edge - O(n) *)
let no_edge lst str = (exists lst str (fun ele-> List.nth ele 1)) = []

(* Returns a list of nodes that has an incoming edge equal to str - O(n^2) *)
let rec get_m lst str =
    match (exists lst str (fun b -> List.nth b 0)) with
    | []   -> []
    | head::tail -> head :: get_m tail str

let rec inner_topo_sort s edges = 
    match s with
    | [] -> if List.is_empty edges 
                then []
                else ["cycle"]
    | head::tail -> 
        let m = get_m edges head in
            let edges = List.filter (fun edge -> not (List.mem edge m)) edges in
                head :: inner_topo_sort (List.fast_sort String.compare ((List.filter (no_edge edges) (List.map (fun edge -> List.nth edge 1) m)) @ tail)) edges

let topo_sort edges =
    let nodes = List.sort_uniq String.compare (List.flatten edges) in
        let s = List.fast_sort String.compare (List.filter (no_edge edges) nodes) in
            inner_topo_sort s edges
(* -------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let rec read_ast ic prev =
match input_line ic with
    | "no_inherits" -> ["Object"; prev] :: read_ast ic ""
    | "inherits"    -> 
        let _ = input_line ic in 
        let c = input_line ic in 
            [c; prev] :: read_ast ic ""
    | line -> read_ast ic line
    | exception End_of_file -> [];;

let classes = (open_in Sys.argv.(1) |> read_ast) "" |> topo_sort in
    if List.exists (String.equal "cycle") classes
        then
            printf "ERROR: 0: Type-Check: inheritance cycle:\n"
        else List.iter (printf "%s\n") classes;
    let t = classes in t;