open Hashtbl

let rec read_ast ic =
match input_line ic with
    | line -> print_endline line;
              read_ast ic
    | exception End_of_file -> ()

let () = read_ast (open_in Sys.argv.(1));