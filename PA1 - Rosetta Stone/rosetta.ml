open Printf

let rec create_set () =  
    match [read_line(); read_line()] with
    | n  -> [n] @ create_set()
    | exception End_of_file -> []

let rec find lst str tf = 
    match lst with
    | [] -> []
    | head::tail ->
        match String.equal str (tf head) with
        | true  -> head::tail
        | false -> find tail str tf

let no_edge lst str = (find lst str (fun b -> List.nth b 1)) = []

let rec get_m lst str =
    match (find lst str (fun b -> List.nth b 0)) with
    | []   -> []
    | head::tail -> head :: get_m tail str

let rec create_l s edges = 
    match s with
    | [] -> 
        if List.is_empty edges 
            then []
            else ["cycle"]
    | head::tail -> 
        let m = get_m edges head in
            let edges = List.filter (fun edge -> not (List.mem edge m)) edges in
                head :: create_l (List.fast_sort String.compare ((List.filter (no_edge edges) (List.map (fun edge -> List.nth edge 1) m)) @ tail)) edges;;

let edges = create_set () in
    let nodes = List.sort_uniq String.compare (List.flatten edges) in
        let s = List.fast_sort String.compare (List.filter (no_edge edges) nodes) in
            let l = create_l s edges in
                if List.exists (String.equal "cycle") l 
                    then printf "cycle"
                    else List.iter (printf "%s\n") l