open Printf;;

let word_size = 8;;

type tac_object = {
    pos : string;           (* Position  *)
    ttype : string;         (* Type      *)
    opr : string;           (* Operation *)
    lhd : string;
    rhd : string;
};;

type tac = 
    | Call     of string list * tac_object
    | Comment  of string
    | Jump     of string
    | Label    of string
    | TAC      of tac_object
;;

type object_id = {
    name : string;
    pos : int;
    obj_type : string;
    obj_body : tac list;
    temp_length : int;
};;

type dispatch = {
    id : object_id;
    formals : string list;
};;

type object_layout = {
    type_tag : int;
    size     : int;
    vtable     : dispatch  list;
    attributes : object_id list;
    attribute_locations : (string, string) Hashtbl.t;
    attr_temp : int;
};;

type location = {
    register : string;
    offset : int;
    in_memory : bool;
}

let ctable = Hashtbl.create 50;;
let cool_names = ["Bool"; "Int"; "IO"; "Object"; "String"];;
let cool_new = Hashtbl.create 50;;
Hashtbl.add cool_new 
    "Bool" 
    ["movq $8, %rsi";
    "movq $4, %rdi";
    "call calloc";
    "## store class tag, object size and vtable pointer";
    "movq $0, 0(%rax)";
    "movq $4, 8(%rax)";
    "movq $Bool..vtable, 16(%rax)";
    "## initialize attributes";
    "## self[3] holds field (raw content) (Int)";
    "movq $0, 24(%rax)";
    "## self[3] (raw content) initializer -- none";
    "ret"]
;;

Hashtbl.add cool_new 
    "IO"
    ["movq $8, %rsi";
    "movq $3, %rdi";
    "call calloc";
    "## store class tag, object size and vtable pointer";
    "movq $1, 0(%rax)";
    "movq $3, 8(%rax)";
    "movq $IO..vtable, 16(%rax)";
    "ret"]
;;

Hashtbl.add cool_new
    "Int"
    ["movq $8, %rsi";
	"movq $4, %rdi";
	"call calloc";
    "## store class tag, object size and vtable pointer";
    "movq $2, 0(%rax)";
    "movq $4, 8(%rax)";
    "movq $Int..vtable, 16(%rax)";
    "## initialize attributes";
    "## self[3] holds field (raw content) (Int)";
    "movq $0, 24(%rax)";
    "## self[3] (raw content) initializer -- none";
    "## return address handling";
    "ret"]
;;

Hashtbl.add cool_new
    "Object"
    ["movq $8, %rsi";
    "movq $3, %rdi";
    "call calloc";
    "## store class tag, object size and vtable pointer";
    "movq $3, 0(%rax)";
    "movq $3, 8(%rax)";
    "movq $Object..vtable, 16(%rax)";
    "ret"]
;;

Hashtbl.add cool_new
    "String"
    ["movq $8, %rsi";
    "movq $4, %rdi";
    "call calloc";
    "## store class tag, object size and vtable pointer";
    "movq $3, 0(%rax)";
    "movq $4, 8(%rax)";
    "movq $String..vtable, 16(%rax)";
    "## initialize attributes";
    "## self[3] holds field (raw content) (String)";
    "movq $the.empty.string, 24(%rax)";
    "## self[3] (raw content) initializer -- none"; 
    "ret"]
;;

let cool_methods = Hashtbl.create 50;;
Hashtbl.add cool_methods
    "abort"
    ["movq $string8, %rdi";
    "call cooloutstr";
    "movl $0, %edi";
    "call exit";]
;;

Hashtbl.add cool_methods
    "copy"
    ["pushq %rbp";
    "movq %rsp, %rbp";
    "movq 16(%rbp), %r12";
    "## return address handling";
    "## method body begins";
    "movq 8(%r12), %r14";
    "movq $8, %rsi";
    "movq %r14, %rdi";
    "call calloc";
    "movq %rax, %r13";
    "pushq %r13";]
;;

Hashtbl.add cool_methods
    "copy.pred"
    ["cmpq $0, %r14";
    "jne copy.body";
    "popq %r13";
    "## return address handling";
    "movq %rbp, %rsp";
    "popq %rbp";
    "ret";]
;;

Hashtbl.add cool_methods
    "copy.body"
    ["movq 0(%r12), %r15";
    "movq %r15, 0(%r13)";
    "addq $8, %r12";
    "addq $8, %r13";
    "subq $1, %r14";
    "jmp copy.pred"]
;;

Hashtbl.add cool_methods
    "type_name"
    ["pushq %rbp";
    "movq %rsp, %rbp";
    "movq 16(%rbp), %r12";
    "## return address handling";
    "## method body begins";
    "## new String";
    "movq $String..new, %r14";
    "call *%r14";
    "## obtain vtable for self object";
    "movq 16(%r12), %r14";
    "## look up type name at offset 0 in vtable";
    "movq 0(%r14), %r14";
    "movq %r14, 24(%r13)";
    "## return address handling";
    "movq %rbp, %rsp";
    "popq %rbp";
    "ret"]
;;

Hashtbl.add cool_methods
    "in_int"
    ["pushq %rbp";
    "movq %rsp, %rbp";
    "movl $1, %esi";
    "movl $4096, %edi";
    "call calloc";
    "pushq %rax";
    "movq %rax, %rdi";
    "movq $4096, %rsi";
    "movq stdin(%rip), %rdx";
    "call fgets"; 
    "popq %rdi";
    "movl $0, %eax";
    "pushq %rax";
    "movq %rsp, %rdx";
    "movq $percent.ld, %rsi";
    "call sscanf";
    "popq %rax";
    "movq $0, %rsi"; 
    "cmpq $2147483647, %rax"; 
    "cmovg %rsi, %rax";
    "cmpq $-2147483648, %rax"; 
    "cmovl %rsi, %rax";
    "movq %rbp, %rsp";
    "popq %rbp";
    "ret"]
;;

Hashtbl.add cool_methods
    "in_string"
    ["pushq %rbp";
    "movq %rsp, %rbp";
    "## new String";
    "movq $String..new, %rax";
    "call *%rax";
    "movq %rax, %r11";
	"call coolgetstr ";
    "movq %rax, 24(%r11)";
    "movq %r11, %rax"]
;;

Hashtbl.add cool_methods
    "out_int"
    ["movq $percent.d, %rdi";
	"movl 8(%rbp), %eax";
	"cdqe";
	"movq %rax, %rsi";
	"movl $0, %eax";
	"call printf";
    "movq 16(%rbp), %rax";
    "subq $16, %rbp";
    "ret"]
;;

Hashtbl.add cool_methods
    "out_string"
    ["movq 8(%rbp), %rax";
    "movq 24(%rax), %r11";
	"movq %r11, %rdi";
	"call cooloutstr";
    "movq 16(%rbp), %rax";
    "ret"]
;;

Hashtbl.add cool_methods
    "concat"
    [""]
;;

Hashtbl.add cool_methods
    "length"
    [""]
;;

Hashtbl.add cool_methods
    "substr"
    [""]
;;

(* Variable Hashtable - For storing attributes/variables/temporaries *)
let ltable = Hashtbl.create 50;; 
let next_location = ref (-1);;

let create_location var =
    let () = next_location := !next_location + 1 in
    let new_loc = 
        if !next_location = -1 then 
            {register = "%rax"; offset = 0; in_memory = false}
        else
            {register = "%rsp"; offset = !next_location * word_size; in_memory = true} in
    Hashtbl.add ltable var new_loc
;; 

let get_location var =
    if Hashtbl.mem ltable var |> not then
        create_location var;
    let loc = Hashtbl.find ltable var in
    if loc.in_memory then
        String.concat "" [string_of_int loc.offset; "("; loc.register; ")"]
    else
        "%rax"
;;

let reset_ltable () = 
    next_location := -1;
    Hashtbl.reset ltable
;;


(* Simple Counter for String Tags *)
let tag = ref (4);;
let next_tag = fun() ->
    tag := !tag + 1; !tag
;;

(* String hashtable - For storing string constants *)
let str_table = Hashtbl.create 50;;
let string_location str = 
    if Hashtbl.mem str_table str then
        Hashtbl.find str_table str
    else
        let new_tag = next_tag () in 
        let () = Hashtbl.add str_table str new_tag in
        new_tag
;;

(* Three-Address Code Generator *)
let while_buffer = ref [];;

(* Simple Counter for Assignments *)

let c = ref (0);;  (* Variable counter *)
let m = ref (-1);;  (* Highest count reached, useful for temporary variable tracking *)
let next_assign = fun() ->
    c := !c + 1; 
    if !m < !c then m := !c; 
    !c |> string_of_int
;;
let unassign = fun() ->
    c := !c - 1;
;;
let max_assign = fun() ->
    let max = !m in 
    m := -1; max
;;

(* Simple Counter for Jumps *)
let jmp = ref (-1);;
let next_jump = fun() ->
    jmp := !jmp + 1; !jmp |> string_of_int
;;

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
    done
;;

let rec convert_tac ic assign =
    let pro_line = input_line ic in
    let dtype = input_line ic in
    let expr = input_line ic in
    match expr with
    | "assign" ->
        ignore (input_line ic);
        let ident = input_line ic in
        let new_tac = { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = ident } in 
        convert_tac ic ident @ [TAC new_tac];
    | "dynamic_dispatch" ->
        let source_lst = convert_tac ic assign in
        ignore (input_line ic);
        let dispatch = input_line ic in
        let argument_length = input_line ic |> int_of_string in
        let arguments = convert_args ic [] [] argument_length in 
        let dispatch_tac = { pos = pro_line; ttype = dtype; opr = "dynamic dispatch"; lhd = assign; rhd = dispatch } in
        for i = 0 to argument_length do unassign() done;
        source_lst @ snd arguments @ [Call (fst arguments, dispatch_tac)];
    | "block" ->
        convert_tac_block ic assign (input_line ic |> int_of_string)
    | "identifier" ->
        ignore (input_line ic);
        let ident = input_line ic in
        [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = ident }];
    | "if" ->
        let cond_tacs = convert_tac ic assign in 
        let then_tacs = convert_tac ic assign in
        let else_tacs = convert_tac ic assign in
        let then_jump = next_jump() in
        let else_jump = next_jump() in
        let exit_jump = next_jump() in
        (* Conditional Check *)
        cond_tacs @
        TAC { pos = ""; ttype = ""; opr = "bt"; lhd = assign; rhd = else_jump } :: 
        TAC { pos = ""; ttype = "Bool"; opr = "not"; lhd = assign; rhd = assign } ::
        TAC { pos = ""; ttype = ""; opr = "bt"; lhd = assign; rhd = then_jump } ::
        (* Then branch tacs *)
        Comment "then branch" :: Label then_jump :: then_tacs @ Jump exit_jump ::
        (* Else branch tacs *)
        Comment "else branch" :: Label else_jump :: else_tacs @ Jump exit_jump ::
        (* End of if statement *)
        Comment "if-join" :: [Label exit_jump]
    | "integer" ->
        [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = input_line ic }];
    | "let" ->
        let let_length = input_line ic |> int_of_string in
        let let_bindings = convert_let ic let_length in 
        let tac_lst = let_bindings @ convert_tac ic assign in
        for i = 1 to let_length do unassign() done;
        tac_lst
    | "isvoid" | "not" | "negate" -> 
        convert_tac ic assign @ [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = "" }];
    | "new" ->
        ignore (input_line ic);
        [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = input_line ic }]
    | "plus" | "minus" | "times" | "divide" | "eq" | "lt" | "le" ->
        let temp_var = next_assign() in
        let lhd_tacs = convert_tac ic temp_var in 
        let rhd_tacs = convert_tac ic assign in
        unassign();
        lhd_tacs @ rhd_tacs @ [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = temp_var }]
    | "self_dispatch" ->
        ignore (input_line ic);
        let dispatch = input_line ic in
        let argument_length = input_line ic |> int_of_string in
        let arguments = convert_args ic [] [] argument_length in
        let dispatch_tac = { pos = pro_line; ttype = dtype; opr = "self dispatch"; lhd = assign; rhd = dispatch } in
        for i = 1 to argument_length do unassign() done;
        snd arguments @ [Call (fst arguments, dispatch_tac)]
    | "static_dispatch" ->
        let source_lst = convert_tac ic assign in
        ignore (input_line ic);
        let parent_class = input_line ic in
        ignore (input_line ic);
        let dispatch = input_line ic in
        let argument_length = input_line ic |> int_of_string in
        let arguments = convert_args ic [] [] argument_length in 
        let dispatch_tac = { pos = pro_line; ttype = dtype; opr = "static dispatch"; 
            lhd = assign; rhd = dispatch } in
        for i = 1 to argument_length do unassign() done;
        source_lst @ snd arguments @ [Call (parent_class :: fst arguments, dispatch_tac)];
    | "string" -> 
        [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = input_line ic }];
    | "true" | "false" -> 
        [TAC { pos = pro_line; ttype = dtype; opr = expr; lhd = assign; rhd = "" }];
    | "while" ->
        let pred_assign = next_assign() in
        let pred_tacs = convert_tac ic pred_assign in
        let body_tacs = convert_tac ic (next_assign()) in
        let pred_jmp = next_jump() in
        let exit_jmp = next_jump() in
        let body_jmp = next_jump() in
        let not_pred_assign = next_assign() in
        let prev_while = !while_buffer in 
        while_buffer := 
            Comment "while-body" ::
            Label body_jmp ::
            body_tacs @
            [Jump pred_jmp];
        Jump pred_jmp ::
        prev_while @
        Comment "while-pred" :: 
        Label pred_jmp :: pred_tacs @
        TAC { pos = ""; ttype = "Bool"; opr = "not"; lhd = not_pred_assign; rhd = pred_assign } ::
        TAC { pos = ""; ttype = ""; opr = "bt"; lhd = not_pred_assign; rhd = exit_jmp } ::
        TAC { pos = ""; ttype = ""; opr = "bt"; lhd = pred_assign; rhd = body_jmp } ::
        Comment "comment while-join" ::
        Label exit_jmp ::
        [TAC { pos = ""; ttype = "Object"; opr = "default"; lhd = assign; rhd = "Object" }]       (* Maybe change this for something else *)
    | _ -> raise (Failure (String.concat " " ["Unknown expression type"; expr]))
and convert_tac_block ic assign length =
    match length with
    | 0 -> []
    | _ -> let new_tac = convert_tac ic assign in
            new_tac @ convert_tac_block ic assign (length - 1)
and convert_args ic arg_locs tac_lst length = 
    match length with
    | 0 -> (List.rev arg_locs), (tac_lst)
    | _ -> 
        let arg_loc = next_assign() in
        let tac = convert_tac ic arg_loc in
        convert_args ic (arg_loc :: arg_locs) (tac_lst @ tac) (length - 1)
and convert_let ic length = 
    match length with
    | 0 -> []
    | _ -> 
        let bind_loc = next_assign() in
        let binding = input_line ic in
        ignore (input_line ic, input_line ic, input_line ic);
        let static_type = input_line ic in
        let binding_tac = (
            if String.equal binding "let_binding_no_init" then
                [TAC { pos = ""; ttype = static_type; opr = "default"; lhd = binding; rhd = bind_loc }]  (* Maybe change this to something else *)
            else convert_tac ic (bind_loc) @ [TAC { pos = ""; ttype = static_type; opr = "let"; lhd = binding; rhd = bind_loc }]
        ) in binding_tac @ convert_let ic (length - 1)
;;

(* implementation_map Handler - We want to store methods so that it can be called upon when needed *)
let rec im_foreach_dispatch ic acc length =
    match length with
    | 0 -> []
    | _ -> 
        let d_name = input_line ic in
        let formal_count = input_line ic |> int_of_string in
        let d_formals = im_foreach_formal ic formal_count in
        let d_origin = input_line ic in
        let tacs = 
        if List.mem d_origin cool_names then 
            let _ = (input_line ic, input_line ic, input_line ic, input_line ic) in []
        else 
            convert_tac ic (next_assign())
        in 
        let id = { name = d_name; pos = acc; obj_type = d_origin; obj_body = tacs; temp_length = max_assign() } in 
        { id = id; formals = d_formals } :: im_foreach_dispatch ic (acc + 1) (length - 1)
and im_foreach_formal ic length =
    match length with
    | 0 -> []
    | _ -> let formal = input_line ic in formal :: im_foreach_formal ic (length - 1);;
;;

let rec im_foreach_class ic length =
    match length with
    | 0 -> ()
    | _ ->
        let class_name = input_line ic in
        let method_count = input_line ic |> int_of_string in
        let methods = im_foreach_dispatch ic 0 method_count in
        let prev = Hashtbl.find ctable class_name in
        Hashtbl.replace ctable class_name {prev with vtable = methods};
        im_foreach_class ic (length - 1);
;;

let parse_implement_map ic =
    ignore (input_line ic);                                                         (* Ignore implement_map comment *)
    let class_count = input_line ic |> int_of_string in
    im_foreach_class ic class_count
;;

(* class_map Handler - We want to store class_map attributes initializations for creating new Objects of each class *)
let rec cm_foreach_attr ic acc length =
    match length with
    | 0 -> []
    | _ -> 
        let init = input_line ic in
        let a_ident = input_line ic in
        let a_type  = input_line ic in
        let attribute =
        if String.equal init "initializer" then
            {name = a_ident; pos = acc; obj_type = a_type; obj_body = convert_tac ic "0"; temp_length = max_assign()}
        else
            {name = a_ident; pos = acc; obj_type = a_type; obj_body = []; temp_length = 0}
        in attribute :: cm_foreach_attr ic (acc + 1) (length - 1);
;;

let rec cm_foreach_class ic length = 
    match length with
    | 0 -> ()
    | _ -> 
        let class_name = input_line ic in
        let attr_count = input_line ic |> int_of_string in
        let attributes = cm_foreach_attr ic 3 attr_count in
        let temp_length = List.fold_left (fun acc e -> if e.temp_length > acc then e.temp_length else acc) 0 attributes in
        let tag = string_location class_name in
        let class_layout =
            if List.mem class_name cool_names then
                { type_tag = tag; size = 3; vtable = []; attributes = []; attribute_locations = Hashtbl.create 0; attr_temp = 0 }
            else
                { type_tag = tag; size = 3 + attr_count; vtable = []; attributes = attributes; attribute_locations = Hashtbl.create 50; attr_temp = temp_length } in
        Hashtbl.add ctable class_name class_layout;
        cm_foreach_class ic (length - 1);
;;

let class_map ic =
    ignore (input_line ic); (* Ignore class_map comment *)
    let class_count = input_line ic |> int_of_string in 
    cm_foreach_class ic class_count;
;;

let rec create_path ic length =
    match length with 
    | 0 -> []
    | _ -> 
        let aclass = input_line ic in 
        ignore (input_line ic); 
        aclass :: create_path ic (length - 1);
;;

let fprintf_vtable oc class_name =
    let class_layout = (Hashtbl.find ctable class_name) in
    fprintf oc "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
    fprintf oc ".globl %s..vtable\n" class_name;
    if (String.length class_name < 14) then
        fprintf oc "%s..vtable:%*s## virtual function table for %s\n" class_name (15 - String.length class_name) "" class_name
    else
        fprintf oc "%s..vtable: ## virtual function table for %s\n" class_name class_name;
    fprintf oc "%24s.quad string%i\n" "" class_layout.type_tag;
    fprintf oc "%24s.quad %s..new\n" "" class_name;
    List.iter (fun e ->
        fprintf oc "%24s.quad %s.%s\n" "" e.id.obj_type e.id.name
    ) class_layout.vtable;
;;

let fprintf_tac oc tac =
    match tac.opr with
    (* Boolean Constants *)
    | "true" ->
        fprintf oc "%24smovq $1, %s\n" "" (get_location tac.lhd); 
    | "false" ->
        fprintf oc "%24smovq $0, %s\n" "" (get_location tac.lhd);
    | "identifier" ->
        fprintf oc "%24smovq %s, %s\n" "" (get_location tac.rhd) (get_location tac.lhd);
    | "integer" -> 
        fprintf oc "%24smovq $%s, %s\n" "" tac.rhd (get_location tac.lhd);
    | "isvoid" -> 
        fprintf oc "%24sandq $0, %s\n" "" (get_location tac.lhd);
    | "new" ->
        if String.equal tac.ttype "Int" || String.equal tac.ttype "Bool" then
            fprintf oc "%24smovq $0, %s\n" "" (get_location tac.lhd)
        else let () = 
            fprintf oc "%24smovq $%s..new, %%rax\n" "" tac.ttype;
            if (Hashtbl.find ctable tac.ttype).attr_temp != 0 then
            (* check if dispatch uses temp vars *)
            fprintf oc "%24scall *%%rax\n" "" in
            (* pop if so *)
            if String.equal tac.lhd "0" |> not then
                fprintf oc "%24smovq %%rax, %s\n" "" (get_location tac.lhd)
    | "negate" ->
        fprintf oc "%24snegq %s\n" "" (get_location tac.lhd);
    | "not" -> 
        fprintf oc "%24sxorq $1, %s\n" "" (get_location tac.lhd);
    | "plus" ->
        fprintf oc "%24saddq %s, %s\n" "" (get_location tac.rhd) (get_location tac.lhd);
    | "minus" ->
        fprintf oc "%24ssubq %s, %s\n" "" (get_location tac.rhd) (get_location tac.lhd);
    | "times" ->
        fprintf oc "%24smulq %s\n" "" (get_location tac.lhd);
        if String.equal tac.lhd "0" |> not then
            fprintf oc "%24smovq %%rax, %s\n" "" (get_location tac.lhd); 
    | "string" ->
        fprintf oc "%24smovq $string%i, %s\n" "" (string_location tac.rhd) (get_location tac.lhd);
    | _ -> fprintf oc "%25s %s %s\n" tac.lhd tac.opr tac.rhd;
;;

let fprintf_dispatch oc class_name info tac =
    match tac.opr with
    | "dynamic dispatch" ->
        let static_type = (* Issue: Static type is not being returned here *)
            if String.equal tac.ttype "SELF_TYPE" then
                class_name
            else
                tac.ttype in
        let dispatch = List.find (fun e -> String.equal e.id.name tac.rhd) (Hashtbl.find ctable static_type).vtable in
        fprintf oc "%24s## obtain vtable from object in %s with static type %s\n" "" (get_location tac.lhd) static_type;
        if (Hashtbl.find ltable tac.lhd).in_memory then (
            fprintf oc "%24smovq %s, %%r15\n" "" (get_location tac.lhd);
            fprintf oc "%24smovq %i(%%r15), %%r15\n" "" (word_size * 2);
            fprintf oc "%24smovq %%r15, %s\n" "" (get_location tac.lhd);
        ) else 
            fprintf oc "%24smovq %i(%s), %s\n" "" (word_size * 2) (get_location tac.lhd) (get_location tac.lhd); 
        if !next_location > 0 then
            fprintf oc "%24spush %%rax\n" "";
        fprintf oc "%24s## lookup %s() at offset %i in vtable\n" "" tac.rhd dispatch.id.pos;
        fprintf oc "%24smovq %i(%%rax), %%rax\n" "" (word_size * dispatch.id.pos);
        fprintf oc "%24scall *%%rax\n" "";
        if String.equal tac.lhd "0" |> not then
            fprintf oc "%24smovq %%rax, %s\n" "" (get_location tac.lhd);
        if !next_location > 0 then
            fprintf oc "%24spop %%rax\n" "";
    | "self dispatch" ->
        let dispatch = List.find (fun e -> String.equal e.id.name tac.rhd) (Hashtbl.find ctable class_name).vtable in
        fprintf oc "%24s## obtain vtable for self object of type %s\n" "" class_name;
        fprintf oc "%24smovq %i(%%r12), %%rax\n" "" (word_size * 2);
        fprintf oc "%24s## lookup %s() at offset %i in vtable\n" "" tac.rhd dispatch.id.pos;
        fprintf oc "%24smovq %i(%%rax), %%rax\n" "" (word_size * dispatch.id.pos);
        fprintf oc "%24scall *%%rax\n" "";
        if String.equal tac.lhd "0" |> not then
            fprintf oc "%24smovq %%rax, %s\n" "" (get_location tac.lhd);
    | "static dispatch" ->
        let dispatch = List.find (fun e -> String.equal e.id.name tac.rhd) (Hashtbl.find ctable (List.hd info)).vtable in
        fprintf oc "%24s## obtain vtable from object in %s with static type %s\n" "" tac.lhd tac.ttype; (* Issue: Static type is not being returned here *)
        fprintf oc "%24smovq %i(%s), %s\n" "" (word_size * 2) tac.lhd tac.lhd; 
        fprintf oc "%24s## lookup %s() at offset %i in vtable\n" "" tac.rhd dispatch.id.pos;
        fprintf oc "%24smovq %i(%%rax), %%rax\n" "" (word_size * dispatch.id.pos);
        fprintf oc "%24scall *%%rax\n" "";
        if String.equal tac.lhd "0" |> not then
            fprintf oc "%24smovq %%rax, %s\n" "" tac.lhd;
    | _ -> raise (Failure "Illegal dispatch operator\n");
;;

let fprintf_attribute_initializer oc class_name attribute = 
    fprintf oc "%24s## self[%i] %s initializer%s\n" "" attribute.pos attribute.name (if attribute.obj_body != [] then "" else " -- none");
    List.iter (fun tacs -> 
        match tacs with
        | TAC tac   -> fprintf_tac oc tac; 
        | Comment c -> fprintf oc "%24s## Comment: %s\n" "" c;
        | Call (dispatch_info, dispatch_tac) -> 
            fprintf_dispatch oc class_name dispatch_info dispatch_tac
        | Jump jmp  -> fprintf oc "%24sJump: %s\n" "" jmp;
        | Label l   -> fprintf oc "%24sLabel: %s\n" "" l; 
    ) attribute.obj_body;
    fprintf oc "%24smovq %%rax, %i(%%r12)\n" "" (word_size * attribute.pos);
;;

let fprintf_attribute oc attribute = 
    fprintf oc "%24s# self[%s] holds field %s (%s)\n" "" (attribute.pos |> string_of_int) attribute.name attribute.obj_type; 
    if List.mem attribute.obj_type ["Bool"; "Int"; "String"] then
        let () = fprintf oc "%24smovq $%s..new, %%rax\n" "" attribute.obj_type in 
        let () = fprintf oc "%24scall *%%rax\n" "" in
        let () = Hashtbl.add ltable attribute.name { register = "%r12"; offset = word_size * attribute.pos; in_memory = true } in
        fprintf oc "%24smovq %%rax, %s\n" "" (get_location attribute.name);
    else
        fprintf oc "%24smovq $0, %i(%%r12)\n" "" (word_size * attribute.pos);
;;

let fprintf_new oc class_name =
    fprintf oc "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
    let class_layout = (Hashtbl.find ctable class_name) in
    fprintf oc ".globl %s..new\n" class_name;
    fprintf oc "%s..new:" class_name;
    if String.length class_name < 18 then
        fprintf oc "%*s## constructor for %s\n" (18 - String.length class_name) "" class_name
    else
        fprintf oc " ## constructor for %s\n" class_name;
    if List.mem class_name cool_names then
        List.iter (fprintf oc "%24s%s\n" "") (Hashtbl.find cool_new class_name)
    else (
        List.iter (fprintf oc "%24s%s\n" "") ["pushq %rbp"; "movq %rsp, %rbp"];
        if (class_layout.attr_temp != 0) then (
            fprintf oc "%24s## stack room for temporaries: %i\n" "" class_layout.attr_temp;
            fprintf oc "%24smovq $%i, %%rsp\n" "" (word_size * (class_layout.attr_temp + (class_layout.attr_temp mod 2))));
        List.iter (fprintf oc "%24s%s\n" "")
            [String.concat "" ["## Create space for new "; class_name; "; stored in %rax"];
             String.concat "" ["movq $"; string_of_int word_size; ", %rsi"];
             String.concat "" ["movq $"; string_of_int class_layout.size; ", %rdi"];
             "call calloc"; 
             "## store class tag, object size and vtable pointer";
             String.concat "" ["movq $"; string_of_int class_layout.type_tag; ", 0(%rax)"];
             String.concat "" ["movq $"; string_of_int class_layout.size; ", "; string_of_int (1 * word_size); "(%rax)"];
             String.concat "" ["movq $"; class_name; "..vtable, "; string_of_int (2 * word_size); "(%rax)"];
             "## self stored in %r12";
             "movq %rax, %r12";
             "## initialize attributes"];
        (* Populate new class function with attribute definition and initialization *)
        List.iter (fprintf_attribute oc) class_layout.attributes;
        List.iter (fprintf_attribute_initializer oc class_name) class_layout.attributes;
        (* Return Handling *)
        List.iter (fprintf oc "%24s%s\n" "") ["## return address handling"; "movq %rbp, %rsp"; "popq %rbp"; "ret"];
        (* Reset location table for next class *)
        reset_ltable();
    );
;;

let fprintf_dispatch oc class_name dispatch =
    fprintf oc ".globl %s.%s\n" class_name dispatch.id.name;
    fprintf oc "%s.%s:\n" class_name dispatch.id.name;
    if String.equal "Object" class_name && String.equal "copy" dispatch.id.name then (
        List.iter (fprintf oc "%24s%s\n" "") (Hashtbl.find cool_methods "copy");
        fprintf oc ".globl copy.pred\n";
        fprintf oc "copy.pred:\n";
        List.iter (fprintf oc "%24s%s\n" "") (Hashtbl.find cool_methods "copy.pred");
        fprintf oc ".globl copy.body\n";
        fprintf oc "copy.body:\n";
        List.iter (fprintf oc "%24s%s\n" "") (Hashtbl.find cool_methods "copy.body")
    ) else if List.mem class_name ["Object"; "IO"; "String"] then
        List.iter (fprintf oc "%24s%s\n" "") (Hashtbl.find cool_methods dispatch.id.name)
    else (
        List.iter (fprintf oc "%24s%s\n" "") ["pushq %rbp"; "movq %rsp, %rbp"];
        if (dispatch.id.temp_length != 0) then (
            fprintf oc "%24s## stack room for temporaries: %i\n" "" dispatch.id.temp_length;
            fprintf oc "%24smovq $%i, %%rsp\n" "" (word_size * (dispatch.id.temp_length + (dispatch.id.temp_length mod 2)))
        );
        List.iter (fun tacs -> 
            match tacs with
            | TAC tac   -> fprintf_tac oc tac; 
            | Comment c -> fprintf oc "%24s## Comment: %s\n" "" c;
            | Call (dispatch_info, dispatch_tac) -> 
                fprintf_dispatch oc class_name dispatch_info dispatch_tac
            | Jump jmp  -> fprintf oc "%24sJump: %s\n" "" jmp;
            | Label l   -> fprintf oc "%24sLabel: %s\n" "" l; 
        ) dispatch.id.obj_body  
    )
;;

let fprintf_class_dispatch oc class_name =
    let class_layout = Hashtbl.find ctable class_name in
    List.iter (fun dispatch ->
        if String.equal dispatch.id.obj_type class_name |> not then () 
        else (
            fprintf oc "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            fprintf_dispatch oc class_name dispatch
        )
    ) class_layout.vtable;
;;

let rec fprintf_string_body oc str =
    if String.equal str "" then 
        fprintf oc ".byte 0\n\n"
    else
        let char = String.get str 0 in
        let () = fprintf oc ".byte %*i # \'%c\'\n" 3 (Char.code char) char in
        fprintf_string_body oc (String.sub str 1 (String.length str - 1));
;;

let fprintf_strings oc path = 
    fprintf oc "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
    fprintf oc "%24s## globl string constants\n" "";
    List.iter (fun e ->
        match e with (label, constant) ->
        fprintf oc ".globl %s\n" label;
        fprintf oc "%s:\n" label;
        fprintf_string_body oc constant;    
    ) [("the.empty.string", ""); ("percent.d", "%ld"); ("percent.ld", " %ld")];
    Hashtbl.iter (fun string constant -> 
        fprintf oc ".globl string%i\n" constant;
        fprintf oc "string%i:\n" constant;
        fprintf_string_body oc string;     
    ) str_table;
    List.iter (fun e ->
        match e with (label, constant) ->
        fprintf oc ".globl %s\n" label;
        fprintf oc "%s:\n" label;
        fprintf_string_body oc constant;
    ) [("void.dispatch", "ERROR: 2: Exception: dispatch on void\\n"); ("stringabort", "abort\\n"); 
            ("out.of.range", "ERROR: 0: Exception: String.substr out of range\\n")]
;;

let main ic oc = 
    class_map ic;
    parse_implement_map ic;
    ignore (input_line ic);
    let path = "Object" :: create_path ic (input_line ic |> int_of_string) in
    List.iter (fprintf_vtable oc) path;
    List.iter (fprintf_new oc) path;
    List.iter (fprintf_class_dispatch oc) path;
    fprintf_strings oc path;
    fprintf oc "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
    fprintf oc ".globl start\n";
    fprintf oc "start:%18s## program begins here\n" "";
    List.iter (fprintf oc "%24s%s\n" "") [".globl main"; ".type main, @function"];
    fprintf oc "main:\n";
    List.iter (fprintf oc "%24s%s\n" "") ["movq $Main..new, %rax"; "call *%rax"; "movq $Main.main, %rax"; "call *%rax"; "movl $0, %edi"; "call exit"];
;;

(*let oc = open_out (String.concat "temp" [(String.sub Sys.argv.(1) 0 (String.length Sys.argv.(1) - 7)); ""]) in*)
let oc = open_out ("temp.s") in
let ic = open_in Sys.argv.(1) in main ic oc;
