open Contdice

(* Read a file and return its contents as a string *)
let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(* Process a single .cdice file *)
let process_file filename =
  Printf.printf "Processing file: %s\n" filename;
  try
    let content = read_file filename in
    Printf.printf "Source:\n%s\n\n" content;
    
    (* Parse the expression *)
    let expr = parse_expr content in
    Printf.printf "Parsed AST:\n%s\n\n" (string_of_expr expr);
    
    (* Type-check the expression *)
    let texpr = elab expr in
    Printf.printf "Typed AST:\n%s\n\n" (string_of_texpr texpr);
    
    (* Print a nice separator *)
    print_endline (String.make 60 '-');
    true
  with
  | Failure msg -> 
      Printf.printf "Error: %s\n\n" msg;
      print_endline (String.make 60 '-');
      false
  | e -> 
      Printf.printf "Unexpected error: %s\n\n" (Printexc.to_string e);
      print_endline (String.make 60 '-');
      false

(* Check if a filename has .cdice extension *)
let has_cdice_extension filename =
  Filename.check_suffix filename ".cdice"

(* Process a directory, finding all .cdice files recursively *)
let rec process_directory path =
  let dir = Unix.opendir path in
  let rec process_entries count errors =
    try
      let entry = Unix.readdir dir in
      if entry = "." || entry = ".." then
        process_entries count errors
      else
        let full_path = Filename.concat path entry in
        let stats = Unix.stat full_path in
        match stats.Unix.st_kind with
        | Unix.S_REG when has_cdice_extension full_path ->
            let success = process_file full_path in
            process_entries (count + 1) (if success then errors else errors + 1)
        | Unix.S_DIR ->
            let sub_count, sub_errors = process_directory full_path in
            process_entries (count + sub_count) (errors + sub_errors)
        | _ ->
            process_entries count errors
    with End_of_file ->
      Unix.closedir dir;
      count, errors
  in
  process_entries 0 0

(* Main function *)
let () =
  let argc = Array.length Sys.argv in
  
  if argc < 2 then
    Printf.printf "Usage: %s <file.cdice | directory>\n" Sys.argv.(0)
  else
    let path = Sys.argv.(1) in
    
    try
      let stats = Unix.stat path in
      match stats.Unix.st_kind with
      | Unix.S_REG ->
          if has_cdice_extension path then
            let _ = process_file path in
            ()
          else
            Printf.printf "Error: File must have .cdice extension\n"
      | Unix.S_DIR ->
          let count, errors = process_directory path in
          Printf.printf "Processed %d files with %d errors\n" count errors
      | _ ->
          Printf.printf "Error: Path is neither a regular file nor a directory\n"
    with
    | Unix.Unix_error(e, _, _) ->
        Printf.printf "Error: %s\n" (Unix.error_message e)