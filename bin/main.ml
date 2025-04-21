open Cmdliner

(* Read a file and return its contents as a string *)
let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(* Helper function to run interpreter N times and summarize *)
let run_interp_and_summarize label expr n_runs : ((int * int), string) result =
  Printf.printf "Running %s version %d times...\n" label n_runs;
  let true_count = ref 0 in
  let false_count = ref 0 in
  try
    for i = 1 to n_runs do
      try 
        match Contdice.Interp.run expr with
        | Contdice.Types.VBool true -> incr true_count
        | Contdice.Types.VBool false -> incr false_count
        | other_val -> 
            let msg = Printf.sprintf "Warning (%s): Expected VBool, got %s after %d runs. Aborting run."
                          label (Contdice.Types.string_of_value other_val) (i-1) in
            raise (Failure msg) (* Use Failure to propagate msg *)
      with 
      | Contdice.Interp.RuntimeError rt_msg -> 
          let msg = Printf.sprintf "Runtime Error during %s run after %d runs: %s. Aborting run."
                          label (i-1) rt_msg in
          raise (Failure msg) (* Use Failure to propagate msg *)
      | Failure msg -> raise (Failure msg) (* Re-raise to outer handler *)
      | e -> 
          let msg = Printf.sprintf "Unexpected Error during %s run after %d runs: %s. Aborting run."
                          label (i-1) (Printexc.to_string e) in
          raise (Failure msg) (* Use Failure to propagate msg *)
    done;
    (* If loop completes without error *) 
    Printf.printf "Summary (%s): True: %d, False: %d\n" label !true_count !false_count;
    Ok (!true_count, !false_count)
  with 
  | Failure final_msg -> Error final_msg (* Catch propagated error message *)

(* Perform a two-proportion z-test *)
let perform_two_proportion_z_test (t_disc, f_disc, e_disc) (t_orig, f_orig, e_orig) _n_runs : bool =
  Printf.printf "\n--- Statistical Comparison ---\n";
  if e_disc > 0 || e_orig > 0 then (
    Printf.printf "Test skipped: Errors occurred during one or both simulation runs.\n";
    false (* Not significantly different if errors occurred *)
  )
  else
    let n_disc = float_of_int (t_disc + f_disc) in
    let n_orig = float_of_int (t_orig + f_orig) in
    
    if n_disc = 0. || n_orig = 0. then (
      Printf.printf "Test skipped: No successful runs for one or both methods.\n";
      false (* Not significantly different if no runs *)
    ) else 
      let p_hat_disc = (float_of_int t_disc) /. n_disc in
      let p_hat_orig = (float_of_int t_orig) /. n_orig in
      
      let total_success = float_of_int (t_disc + t_orig) in
      let total_n = n_disc +. n_orig in
      let p_hat_pool = total_success /. total_n in
      
      Printf.printf "Proportion True (Discretized): %.4f\n" p_hat_disc;
      Printf.printf "Proportion True (Original):   %.4f\n" p_hat_orig;
      Printf.printf "Pooled Proportion:            %.4f\n" p_hat_pool;
      
      let numerator = p_hat_disc -. p_hat_orig in
      let denominator_squared = p_hat_pool *. (1. -. p_hat_pool) *. (1. /. n_disc +. 1. /. n_orig) in
      
      if denominator_squared <= 0. then (
        if abs_float numerator < 1e-9 then (
          Printf.printf "Z-score: 0.0 (Proportions are identical)\n";
          Printf.printf "Conclusion: No statistically significant difference found (alpha=0.05).\n";
          false
        ) else (
          Printf.printf "Z-score: Undefined (Extremely different proportions - one 0%%, one 100%%)\n";
          Printf.printf "Conclusion: Statistically significant difference found (alpha=0.05).\n";
          true
        )
      ) else (
        let z_score = numerator /. sqrt denominator_squared in
        Printf.printf "Z-score: %.4f\n" z_score;
        
        (* Compare against critical value for alpha = 0.01 (two-tailed) *)
        let critical_z = 2.576 in 
        if abs_float z_score > critical_z then (
          Printf.printf "Conclusion: Statistically significant difference found (alpha=0.01).\n";
          true
        ) else (
          Printf.printf "Conclusion: No statistically significant difference found (alpha=0.01).\n";
          false
        )
      )

(* Process a single .cdice file *)
let process_file filename : ( ((int * int) * (int * int)) option, string) result = (* Return Result type *)
  Printf.printf "Processing file: %s\n" filename;
  try
    let content = read_file filename in
    Printf.printf "Source:\n%s\n\n" content;
    
    let expr = Contdice.Parse.parse_expr content in
    Printf.printf "Parsed AST (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_expr expr);
    
    let texpr = Contdice.elab expr in
    Printf.printf "Typed AST (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_texpr texpr);
    
    let discretized_expr = Contdice.discretize texpr in
    Printf.printf "Discretized Program (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_expr discretized_expr);

    (* Feed to result to dice *)
    let discretized_expr_text = Contdice.discretize texpr in
    Printf.printf "Discretized Program (Plaintext):\n%s\n\n" (Contdice.Util.string_of_expr discretized_expr_text);


    let n_runs = 1000000 in 
    match run_interp_and_summarize "Discretized" discretized_expr n_runs with
    | Error msg -> Error msg (* Propagate runtime error *)
    | Ok (t_disc, f_disc) -> 
        match run_interp_and_summarize "Original (Sampling)" expr n_runs with
        | Error msg -> Error msg (* Propagate runtime error *)
        | Ok (t_orig, f_orig) ->
            (* Both runs succeeded, perform test *) 
            let significant_difference = perform_two_proportion_z_test (t_disc, f_disc, 0) (t_orig, f_orig, 0) n_runs in 
            print_endline (String.make 60 '-');
            let diff_details = 
              if significant_difference then Some ((t_disc, f_disc), (t_orig, f_orig))
              else None
            in
            Ok diff_details (* Return Ok on success *)
  with
  | Failure msg -> 
      let err_msg = Printf.sprintf "Error processing file '%s': %s" filename msg in
      print_endline (String.make 60 '-');
      Error err_msg (* Return Error on Failure *)
  | e -> 
      let err_msg = Printf.sprintf "Unexpected error processing file '%s': %s" filename (Printexc.to_string e) in
      print_endline (String.make 60 '-');
      Error err_msg (* Return Error on other exceptions *)

(* Check if a filename has .cdice extension *)
let has_cdice_extension filename =
  Filename.check_suffix filename ".cdice"

(* Process a directory, finding all .cdice files recursively *)
let rec process_directory path : (int * (string * string) list * (string * (int * int) * (int * int)) list) = (* Return (count, errors_list, diff_details_list) *)
  let dir = Unix.opendir path in
  let rec process_entries count errors_list diff_details_list =
    try
      let entry = Unix.readdir dir in
      if entry = "." || entry = ".." then
        process_entries count errors_list diff_details_list
      else
        let full_path = Filename.concat path entry in
        let stats = Unix.stat full_path in
        match stats.Unix.st_kind with
        | Unix.S_REG when has_cdice_extension full_path ->
            (match process_file full_path with
             | Ok diff_details_opt ->
                 let new_diff_details_list = 
                   match diff_details_opt with
                   | Some ((t_d, f_d), (t_o, f_o)) -> (full_path, (t_d, f_d), (t_o, f_o)) :: diff_details_list
                   | None -> diff_details_list
                 in
                 process_entries (count + 1) errors_list new_diff_details_list (* Pass errors_list *)
             | Error msg -> 
                 process_entries (count + 1) ((full_path, msg) :: errors_list) diff_details_list (* Pass updated errors_list *)
            )
        | Unix.S_DIR ->
            let sub_count, sub_errors_list, sub_diff_details_list = process_directory full_path in
            process_entries (count + sub_count) (sub_errors_list @ errors_list) (sub_diff_details_list @ diff_details_list) (* Pass combined errors_list *)
        | _ ->
            process_entries count errors_list diff_details_list (* Pass errors_list *)
    with 
    | Unix.Unix_error (e, _, p) -> 
        let msg = Printf.sprintf "Error processing %s: %s" p (Unix.error_message e) in
        Printf.eprintf "%s\n" msg; 
        process_entries count ((p, msg) :: errors_list) diff_details_list (* Pass updated errors_list *)
    | End_of_file -> 
        Unix.closedir dir;
        (count, errors_list, diff_details_list)
  in
  process_entries 0 [] [] (* Start with empty lists *)

(* Main command execution logic *) 
let run_contdice path =
  try
    let stats = Unix.stat path in
    match stats.Unix.st_kind with
    | Unix.S_REG ->
        if has_cdice_extension path then (
          match process_file path with
          | Ok diff_details_opt ->
              (match diff_details_opt with
               | Some ((t_disc, f_disc), (t_orig, f_orig)) ->
                   Printf.printf "\n*** Statistically significant difference detected for this file. ***\n";
                   Printf.printf "    Discretized: True=%d, False=%d\n" t_disc f_disc;
                   Printf.printf "    Original:    True=%d, False=%d\n" t_orig f_orig
               | None ->
                   Printf.printf "\n*** No statistically significant difference detected for this file. ***\n"
              )
          | Error msg -> 
              Printf.printf "\n*** Error processing file: %s ***\n" msg
        ) else
          Printf.eprintf "Error: File must have .cdice extension: %s\n" path
    | Unix.S_DIR ->
        let count, errors_list, diff_details_list = process_directory path in
        Printf.printf "\n=== Directory Processing Summary ===\n";
        Printf.printf "Processed %d files.\n" count;
        
        if errors_list <> [] then (
          Printf.printf "Errors occurred in %d files:\n" (List.length errors_list);
          List.iter 
            (fun (filename, msg) -> Printf.printf "- %s: %s\n" filename msg)
            (List.rev errors_list)
        );

        if diff_details_list = [] then
          Printf.printf "No statistically significant differences detected in any successfully processed file.\n"
        else (
          Printf.printf "Statistically significant differences detected in the following files:\n";
          List.iter 
            (fun (filename, (t_disc, f_disc), (t_orig, f_orig)) ->
              Printf.printf "- %s (Disc: T=%d F=%d; Orig: T=%d F=%d)\n" 
                           filename t_disc f_disc t_orig f_orig
            ) 
            (List.rev diff_details_list) 
        )
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