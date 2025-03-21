open Printf;;

(* Three-Address Code Generator *)


let operations = ["plus", "+"; "minus", "-"; "times", "*"; "divide", "/"; "eq", "="; "lt", "<"; "le", "<="];;
let while_buffer = ref [];;
let tac_name = ref "";; 
let c = ref (-1);;

let rec skip_to_parent ic = 
    let line = input_line ic in
    let seeker = pos_in ic in
    if String.equal line "parent_map" then
        let _ = input_line ic, input_line ic in
        try int_of_string (input_line ic) |> ignore; skip_to_parent ic;
        with Failure _ -> seek_in ic seeker
    else skip_to_parent ic
and skip_to_ast ic = 
    skip_to_parent ic;
    for i = 1 to (input_line ic |> int_of_string) + 1 do
        ignore (input_line ic, input_line ic);
    done;;

let rec first_method ic = 
    let line = input_line ic in
    if String.equal line "method" then let _ = input_line ic in input_line ic
    else first_method ic;;

(* The name? of the tac : (Class name)_(method name)_ *)

let set_tac_name str = tac_name := str;;
let get_tac_name = fun() -> !tac_name;;


(* Simple Counter for Assignments *)

let next_assign = fun() ->
    c := !c + 1; String.concat "" ["t$"; !c |> string_of_int];;


(* Simple Counter for Jumps *)

let jmp = ref 0;;
let next_jump = fun() ->
    jmp := !jmp + 1; String.concat "" [get_tac_name(); !jmp |> string_of_int];;

(* Main Program, outputs tac to file.cl-tac *)
let rec out_tac ic assign =
    ignore (input_line ic, input_line ic);                                          (* Program Line and dynamic type are ignored *)
    let expr = input_line ic in
    match expr with
    | "assign" ->
        ignore (input_line ic);
        let ident = input_line ic in
        out_tac ic ident @ [String.concat "" [assign; " <- "; ident]];
    | "dynamic_dispatch" ->
        let caller_tac = out_tac ic (next_assign()) in
        ignore (input_line ic);
        let dispatch = input_line ic in
        let formals = convert_dispatch ic [] [] (input_line ic |> int_of_string) in 
        snd formals @
        caller_tac @
        [String.concat " " ([assign; "<- call"; dispatch] @ fst formals)]
    | "block" ->
        let block_length = input_line ic |> int_of_string in
        let body = block_body ic block_length in
        let last = out_tac ic assign in body @ last    
    | "identifier" ->
        ignore (input_line ic);
        let ident = input_line ic in
        String.concat " " [assign; "<-"; ident] :: [];
    | "if" ->
        let then_assign = next_assign() in
        let cond_tac = out_tac ic then_assign in 
        let then_tac = out_tac ic (assign) in
        let else_tac = out_tac ic (assign) in
        let else_assign = next_assign() in 
        let then_jump = next_jump() in
        let else_jump = next_jump() in
        let exit_jump = next_jump() in
        cond_tac @
        String.concat " " [else_assign; "<- not"; then_assign] ::
        String.concat " " ["bt"; else_assign; else_jump] ::
        String.concat " " ["bt"; then_assign; then_jump] ::
        "comment then branch" ::
        String.concat " " ["label"; then_jump] ::
        then_tac @
        String.concat " " ["jmp"; exit_jump] ::
        "comment else branch" ::
        String.concat " " ["label"; else_jump] ::
        else_tac @
        String.concat " " ["jmp"; exit_jump] ::
        "comment if-join" ::
        [String.concat " " ["label"; exit_jump]]
    | "integer" -> String.concat " " [assign; "<- int"; input_line ic] :: [];
    | "let" ->
        let let_length = input_line ic |> int_of_string in
        let let_bindings = convert_let ic let_length in 
        let_bindings @ out_tac ic assign;
    | "isvoid" | "not" -> 
        let new_assign = next_assign() in 
        out_tac ic (new_assign) @ [String.concat " " [assign; "<-"; expr; new_assign]];
    | "negate" -> 
        let new_assign = next_assign() in 
        out_tac ic (new_assign) @ [String.concat " " [assign; "<- ~"; new_assign]];
    | "new" ->
        ignore (input_line ic);
        [String.concat " " [assign; "<- new";input_line ic]]
    | "plus" | "minus" | "times" | "divide" | "eq" | "lt" | "le" ->
        let operation = List.find (fun ele -> String.equal (fst ele) expr) operations |> snd in
        let lhd = next_assign() in let lhd_tac = out_tac ic lhd in
        let rhd = next_assign() in let rhd_tac = out_tac ic rhd in
        lhd_tac @ rhd_tac @ [String.concat " " [assign; "<-"; operation; lhd; rhd]]
    | "self_dispatch" ->
        ignore (input_line ic);
        let dispatch = input_line ic in
        let formals = convert_dispatch ic [] [] (input_line ic |> int_of_string) in 
        snd formals @ [String.concat " " ([assign; "<- call"; dispatch] @ fst formals)]
    | "static_dispatch" ->
        let caller_tac = out_tac ic (next_assign()) in
        ignore (input_line ic, input_line ic, input_line ic);
        let dispatch = input_line ic in
        let formals = convert_dispatch ic [] [] (input_line ic |> int_of_string) in 
        snd formals @
        caller_tac @
        [String.concat " " ([assign; "<- call"; dispatch] @ fst formals)]
    | "string" -> 
        String.concat " " [assign; "<- string"] :: [input_line ic];
    | "true" | "false" -> 
        [String.concat " " [assign; "<- bool"; expr]];
    | "while" ->
        let pred_assign = next_assign() in
        let pred_tac = out_tac ic pred_assign in
        let body_tac = out_tac ic (next_assign()) in
        let pred_jmp = next_jump() in
        let exit_jmp = next_jump() in
        let body_jmp = next_jump() in
        let not_pred_assign = next_assign() in
        let prev_while = !while_buffer in 
        while_buffer := 
            "comment while-body" ::
            String.concat " " ["label"; body_jmp] ::
            body_tac @
            [String.concat " " ["jmp"; pred_jmp]];
        String.concat " " ["jmp"; pred_jmp] ::
        prev_while @
        "comment while-pred" :: 
        String.concat " " ["label"; pred_jmp] :: pred_tac @
        String.concat " " [not_pred_assign; "<- not"; pred_assign] ::
        String.concat " " ["bt"; not_pred_assign; exit_jmp] ::
        String.concat " " ["bt"; pred_assign;     body_jmp] ::
        "comment while-join" ::
        String.concat " " ["label"; exit_jmp] ::
        [String.concat " " [assign; "<- default Object"]]
    | _ -> raise (Failure (String.concat " " ["Unknown expression type"; expr]))
and block_body ic length =
    match length with
    | 1 -> []
    | _ -> let new_tac = out_tac ic (next_assign()) in
            new_tac @ block_body ic (length - 1)
and convert_dispatch ic assignments tacs length = 
    match length with
    | 0 -> assignments, tacs
    | _ -> 
        let formal_assign = next_assign() in
        let formal_tac = out_tac ic formal_assign in
        convert_dispatch ic (assignments @ [formal_assign]) (tacs @ formal_tac) (length - 1)
and convert_let ic length = 
    match length with
    | 0 -> []
    | _ -> 
        let binding = input_line ic in
        ignore (input_line ic, input_line ic, input_line ic);
        let static_type = input_line ic in
        let binding_tac = (
            if String.equal binding "let_binding_no_init" then
                [String.concat " " [next_assign(); "<- default"; static_type]]
            else out_tac ic (next_assign())
        ) in binding_tac @ convert_let ic (length - 1);;
let main ic = 
    skip_to_ast ic;
    let a_class = input_line ic in 
    let a_method = first_method ic in
    String.concat "_" [a_class; a_method; ""] |> set_tac_name;
    ignore (input_line ic, input_line ic, input_line ic);
    let oc = open_out (String.concat "tac" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 4)); ""]) in
    let generated_tac = out_tac ic (next_assign()) in
    fprintf oc "comment start\nlabel %s0\n" (get_tac_name());
    List.iter (fprintf oc "%s\n") generated_tac;
    fprintf oc "return t$0\n";
    List.iter (fprintf oc "%s\n") !while_buffer;;

let ic = open_in Sys.argv.(1) in main ic;