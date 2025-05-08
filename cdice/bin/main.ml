open Cmdliner
open Contdice (* Open Contdice to access its modules like Interp *)
exception ObserveFailure = Contdice.Interp.ObserveFailure (* Explicit alias for the exception *)

(* Read a file and return its contents as a string *)
let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(* Helper function to run interpreter N times and summarize *)
let run_interp_and_summarize ~print_all label expr n_runs : ((int * int * int), string) result =
  if print_all then Printf.printf "Running %s version %d times...\n" label n_runs;
  let true_count = ref 0 in
  let false_count = ref 0 in
  let observe_failures = ref 0 in
  try
    for i = 1 to n_runs do
      try 
        match Interp.run expr with
        | Types.VBool true -> incr true_count
        | Types.VBool false -> incr false_count
        | Types.VUnit -> ()
        | other_val -> 
            let msg = Printf.sprintf "Warning (%s): Expected VBool or VUnit, got %s after %d runs. Aborting run."
                          label (Types.string_of_value other_val) (i-1) in
            raise (Failure msg)
      with 
      | ObserveFailure -> incr observe_failures
      | Interp.RuntimeError rt_msg -> 
          let msg = Printf.sprintf "Runtime Error during %s run after %d runs: %s. Aborting run."
                          label (i-1) rt_msg in
          raise (Failure msg)
      | Failure msg -> raise (Failure msg)
      | e -> 
          let msg = Printf.sprintf "Unexpected Error during %s run after %d runs: %s. Aborting run."
                          label (i-1) (Printexc.to_string e) in
          raise (Failure msg)
    done;
    if print_all then 
      Printf.printf "Summary (%s): True: %d, False: %d, Observe Failures: %d\n" label !true_count !false_count !observe_failures;
    Ok (!true_count, !false_count, !observe_failures)
  with 
  | Failure final_msg -> Error final_msg

(* Perform a two-proportion z-test *)
(* Note: e_disc and e_orig are for other errors, o_disc and o_orig for observe failures *)
let perform_two_proportion_z_test ~print_all (t_disc, f_disc, e_disc, o_disc) (t_orig, f_orig, e_orig, o_orig) _n_runs : bool =
  if print_all then Printf.printf "\n--- Statistical Comparison ---\n";

  (* If observe failures occurred, print a note. This doesn't skip the test. *)
  if print_all && (o_disc > 0 || o_orig > 0) then (
    Printf.printf "Note: Observe failures occurred and were filtered out before statistical comparison.\n";
    Printf.printf "  Discretized: %d observe failures out of %d total attempts (True: %d, False: %d, OFail: %d)\n" o_disc (t_disc + f_disc + o_disc) t_disc f_disc o_disc;
    Printf.printf "  Original:    %d observe failures out of %d total attempts (True: %d, False: %d, OFail: %d)\n" o_orig (t_orig + f_orig + o_orig) t_orig f_orig o_orig;
  );

  (* Skip test if any (non-observe) runtime errors occurred *)
  if e_disc > 0 || e_orig > 0 then (
    if print_all then Printf.printf "Test skipped: Runtime errors occurred during one or both simulation runs.\n";
    false
  )
  else
    let n_disc = float_of_int (t_disc + f_disc) in
    let n_orig = float_of_int (t_orig + f_orig) in
    
    if n_disc = 0. || n_orig = 0. then (
      if print_all then Printf.printf "Test skipped: No successful runs for one or both methods.\n";
      false
    ) else 
      let p_hat_disc = (float_of_int t_disc) /. n_disc in
      let p_hat_orig = (float_of_int t_orig) /. n_orig in
      
      let total_success = float_of_int (t_disc + t_orig) in
      let total_n = n_disc +. n_orig in
      let p_hat_pool = total_success /. total_n in
      
      if print_all then (
        Printf.printf "Proportion True (Discretized): %.4f\n" p_hat_disc;
        Printf.printf "Proportion True (Original):   %.4f\n" p_hat_orig;
        Printf.printf "Pooled Proportion:            %.4f\n" p_hat_pool);
      
      let numerator = p_hat_disc -. p_hat_orig in
      let denominator_squared = p_hat_pool *. (1. -. p_hat_pool) *. (1. /. n_disc +. 1. /. n_orig) in
      
      if denominator_squared <= 0. then (
        if print_all then (
          if abs_float numerator < 1e-9 then (
            Printf.printf "Z-score: 0.0 (Proportions are identical)\n";
            Printf.printf "Conclusion: No statistically significant difference found (alpha=0.05).\n"
          ) else (
            Printf.printf "Z-score: Undefined (Extremely different proportions - one 0%%, one 100%%)\n";
            Printf.printf "Conclusion: Statistically significant difference found (alpha=0.05).\n"
          )
        );
        abs_float numerator >= 1e-9
      ) else (
        let z_score = numerator /. sqrt denominator_squared in
        if print_all then Printf.printf "Z-score: %.4f\n" z_score;
        
        let critical_z = 2.576 in 
        if print_all then (
          if abs_float z_score > critical_z then
            Printf.printf "Conclusion: Statistically significant difference found (alpha=0.01).\n"
          else
            Printf.printf "Conclusion: No statistically significant difference found (alpha=0.01).\n"
        );
        abs_float z_score > critical_z
      )

(* Process a single .cdice file *)
let process_file ~print_all ~to_sppl filename : ( ((int * int * int) * (int * int * int)) option, string) result =
  if print_all then Printf.printf "Processing file: %s\n" filename;
  try
    let content = read_file filename in
    let expr = Contdice.Parse.parse_expr content in
    
    if to_sppl then (
      let sppl_code = Contdice.Pretty.cdice_expr_to_sppl_prog expr in (* Call Pretty module *)
      print_endline sppl_code;
      Ok None (* Indicate success, no diff details *) 
    ) else (
      (* Original logic: pretty print, elab, discretize, compare *)
      if print_all then Printf.printf "Source:\n%s\n\n" content;
      
      let texpr = Contdice.elab_bool expr in 
      if print_all then Printf.printf "Typed AST (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_texpr texpr);
      
      let discretized_expr = Contdice.discretize texpr in
      if print_all then (
        Printf.printf "Discretized Program (Pretty):\n%s\n\n" (Contdice.Pretty.string_of_expr discretized_expr);
        Printf.printf "Discretized Program (Plaintext):\n%s\n\n" (Contdice.Util.string_of_expr discretized_expr)
      ) else (
        Printf.printf "%s\n" (Contdice.Util.string_of_expr discretized_expr)
      );

      if not print_all then Ok None else
      let n_runs = 1000000 in 
      match run_interp_and_summarize ~print_all "Discretized" discretized_expr n_runs with
      | Error msg -> Error msg
      | Ok (t_disc, f_disc, o_disc) -> 
          match run_interp_and_summarize ~print_all "Original (Sampling)" expr n_runs with
          | Error msg -> Error msg
          | Ok (t_orig, f_orig, o_orig) ->
              let significant_difference = 
                perform_two_proportion_z_test ~print_all (t_disc, f_disc, 0, o_disc) (t_orig, f_orig, 0, o_orig) n_runs 
              in 
              if print_all then print_endline (String.make 60 '-');
              let diff_details = 
                if significant_difference then Some (((t_disc, f_disc, o_disc), (t_orig, f_orig, o_orig)))
                else None
              in
              Ok diff_details
    )
  with
  | Failure msg -> 
      let err_msg = Printf.sprintf "Error processing file '%s': %s" filename msg in
      if print_all then print_endline (String.make 60 '-');
      Error err_msg
  | e -> 
      let err_msg = Printf.sprintf "Unexpected error processing file '%s': %s" filename (Printexc.to_string e) in
      if print_all then print_endline (String.make 60 '-');
      Error err_msg

(* Check if a filename has .cdice extension *)
let has_cdice_extension filename =
  Filename.check_suffix filename ".cdice"

(* Process a directory, finding all .cdice files recursively *)
let rec process_directory ~print_all ~to_sppl path : (int * (string * string) list * (string * (int * int * int) * (int * int * int)) list) =
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
            (match process_file ~print_all ~to_sppl full_path with
             | Ok diff_details_opt ->
                 let new_diff_details_list = 
                   match diff_details_opt with
                   | Some ((t_d, f_d, o_d), (t_o, f_o, o_o)) -> 
                       (full_path, (t_d, f_d, o_d), (t_o, f_o, o_o)) :: diff_details_list
                   | None -> diff_details_list
                 in
                 process_entries (count + 1) errors_list new_diff_details_list
             | Error msg -> 
                 process_entries (count + 1) ((full_path, msg) :: errors_list) diff_details_list
            )
        | Unix.S_DIR ->
            let sub_count, sub_errors_list, sub_diff_details_list = process_directory ~print_all ~to_sppl full_path in
            process_entries (count + sub_count) (sub_errors_list @ errors_list) (sub_diff_details_list @ diff_details_list)
        | _ ->
            process_entries count errors_list diff_details_list
    with 
    | Unix.Unix_error (e, _, p) -> 
        let msg = Printf.sprintf "Error processing %s: %s" p (Unix.error_message e) in
        if print_all then Printf.eprintf "%s\n" msg; 
        process_entries count ((p, msg) :: errors_list) diff_details_list
    | End_of_file -> 
        Unix.closedir dir;
        (count, errors_list, diff_details_list)
  in
  process_entries 0 [] []

(* Main command execution logic *) 
let run_contdice path print_all to_sppl =
  try
    let stats = Unix.stat path in
    match stats.Unix.st_kind with
    | Unix.S_REG ->
        if has_cdice_extension path then (
          match process_file ~print_all ~to_sppl path with
          | Ok diff_details_opt ->
              if not to_sppl && print_all then (
                match diff_details_opt with
                | Some ((t_disc, f_disc, o_disc), (t_orig, f_orig, o_orig)) ->
                    Printf.printf "\n*** Statistically significant difference detected for this file. ***\n";
                    Printf.printf "    Discretized: True=%d, False=%d, ObserveFailures=%d\n" t_disc f_disc o_disc;
                    Printf.printf "    Original:    True=%d, False=%d, ObserveFailures=%d\n" t_orig f_orig o_orig;
                | None ->
                    Printf.printf "\n*** No statistically significant difference detected for this file. ***\n"
              )
          | Error msg -> 
              Printf.printf "\n*** Error processing file: %s ***\n" msg
        ) else
          Printf.eprintf "Error: File must have .cdice extension: %s\n" path
    | Unix.S_DIR ->
        let count, errors_list, diff_details_list = process_directory ~print_all ~to_sppl path in
        if print_all then (
          Printf.printf "\n=== Directory Processing Summary ===\n";
          Printf.printf "Processed %d files.\n" count;
          
          if errors_list <> [] then (
            Printf.printf "Errors occurred in %d files:\n" (List.length errors_list);
            List.iter 
              (fun (filename, msg) -> Printf.printf "- %s: %s\n" filename msg)
              (List.rev errors_list)
          );

          if diff_details_list <> [] then (
            Printf.printf "Statistically significant differences detected in the following files:\n";
            List.iter 
              (fun (filename, (t_disc, f_disc, o_disc), (t_orig, f_orig, o_orig)) ->
                Printf.printf "- %s (Disc: T=%d F=%d OFail=%d; Orig: T=%d F=%d OFail=%d)\n" 
                             filename t_disc f_disc o_disc t_orig f_orig o_orig
              ) 
              (List.rev diff_details_list) 
          )
        )
    | _ -> (* Catch-all for other file types *)
        Printf.eprintf "Error: Path is not a regular file or a directory: %s\n" path
  with
  | Failure msg -> 
      Printf.eprintf "Error: %s\n" msg
  | e -> 
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e)

(* Cmdliner term definition *) 
let path_arg = 
  let doc = "The .cdice file or directory to process." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let print_all_arg =
  let doc = "Print detailed output including statistics and intermediate representations." in
  Arg.(value & flag & info ["print-all"] ~doc)

let to_sppl_arg =
  let doc = "Convert the input program to SPPL and print it, skipping other processing." in
  Arg.(value & flag & info ["to-sppl"] ~doc)

let contdice_t = Term.(const run_contdice $ path_arg $ print_all_arg $ to_sppl_arg)

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