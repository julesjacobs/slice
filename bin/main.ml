open Cmdliner

(* Read a file and return its contents as a string *)
let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(* Helper function to run interpreter N times and summarize *)
let run_interp_and_summarize label expr n_runs =
  Printf.printf "Running %s version %d times...\n" label n_runs;
  let true_count = ref 0 in
  let false_count = ref 0 in
  let error_count = ref 0 in
  let ran_successfully = ref true in
  (try
     for _ = 1 to n_runs do
       try 
         match Contdice.Interp.run expr with
         | Contdice.Types.VBool true -> incr true_count
         | Contdice.Types.VBool false -> incr false_count
         | other_val -> 
             Printf.eprintf "Warning (%s): Expected VBool, got %s. Stopping summary.\n" 
                            label (Contdice.Types.string_of_value other_val);
             raise Exit (* Stop summarizing *)
       with 
       | Contdice.Interp.RuntimeError msg -> 
           Printf.eprintf "Runtime Error during %s run: %s. Stopping summary.\n" label msg;
           error_count := n_runs; (* Mark all as errors if one fails early *)
           raise Exit
       | Exit -> raise Exit (* Propagate stop signal *)
       | e -> 
           Printf.eprintf "Unexpected Error during %s run: %s. Stopping summary.\n" label (Printexc.to_string e);
           error_count := n_runs; 
           raise Exit
     done
   with Exit -> ran_successfully := false);

  if !ran_successfully then
    Printf.printf "Summary (%s): True: %d, False: %d\n" label !true_count !false_count
  else 
    Printf.printf "Summary (%s): Run aborted due to error after %d successful runs (True: %d, False: %d). Remaining %d runs counted as errors.\n" 
                    label (!true_count + !false_count) !true_count !false_count (!error_count - !true_count - !false_count);
  (!true_count, !false_count, !error_count)

(* Process a single .cdice file *)
let process_file filename =
  Printf.printf "Processing file: %s\n" filename;
  try
    let content = read_file filename in
    Printf.printf "Source:\n%s\n\n" content;
    
    let expr = Contdice.Parse.parse_expr content in
    Printf.printf "Parsed AST (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_expr expr);
    
    let texpr = Contdice.elab_bool expr in 
    Printf.printf "Typed AST (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_texpr texpr);
    
    let discretized_expr = Contdice.discretize texpr in
    Printf.printf "Discretized Program (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_expr discretized_expr);

    (* Run both versions many times *)
    let n_runs = 1000 in
    let _ = run_interp_and_summarize "Discretized" discretized_expr n_runs in
    let _ = run_interp_and_summarize "Original (Sampling)" expr n_runs in 
    
    print_endline (String.make 60 '-');
    true
  with
  | Failure msg -> 
      Printf.printf "Error processing file '%s': %s\n\n" filename msg;
      print_endline (String.make 60 '-');
      false
  | e -> 
      Printf.printf "Unexpected error processing file '%s': %s\n\n" filename (Printexc.to_string e);
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
    with 
    | Unix.Unix_error (e, _, p) -> 
        Printf.eprintf "Error processing %s: %s\n" p (Unix.error_message e); 
        process_entries count (errors + 1)
    | End_of_file -> 
        Unix.closedir dir;
        count, errors
  in
  process_entries 0 0

(* Main command execution logic *) 
let run_contdice path = (* Reverted: path is required *)
  try
    let stats = Unix.stat path in
    match stats.Unix.st_kind with
    | Unix.S_REG ->
        if has_cdice_extension path then
          let _ = process_file path in
          ()
        else
          Printf.eprintf "Error: File must have .cdice extension: %s\n" path
    | Unix.S_DIR ->
        let count, errors = process_directory path in
        Printf.printf "Processed %d files with %d errors in directory %s\n" count errors path
    | _ ->
        Printf.eprintf "Error: Path is neither a regular file nor a directory: %s\n" path
  with
  | Unix.Unix_error(e, _, p) ->
      Printf.eprintf "Error accessing path %s: %s\n" p (Unix.error_message e)
  | e -> 
      Printf.eprintf "An unexpected error occurred while processing path: %s\n" (Printexc.to_string e); 
      Printexc.print_backtrace stderr

(* Cmdliner term definition *) 
let path_arg = 
  let doc = "The .cdice file or directory to process." in (* Reverted doc *)
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc) (* Reverted to required *) 

let contdice_t = Term.(const run_contdice $ path_arg)

let cmd = 
  let doc = "Process and analyze ContDice files." in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bugs to <your-bug-reporting-address>"; 
  ] in
  let info = Cmd.info "contdice_main" ~version:"%%VERSION%%" ~doc ~exits:Cmd.Exit.defaults ~man in
  Cmd.v info contdice_t

(* Main entry point *) 
let () = exit (Cmd.eval cmd)