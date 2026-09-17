(* Full-pipeline tests for the assertion language.
   Each case compiles test/pipeline/<name>.anvil with the library and checks
   either that the generated SV contains the expected lines (regular
   expressions, so wire numbering does not matter) or that compilation fails
   with the expected message.  Run with `dune test`. *)

type expect =
  | Emits of string list      (* regexes that must each match the generated SV *)
  | Fails of string           (* substring that must appear in the error *)

let cases = [
  (* Str syntax: ( ) | are literal; \( \) group; \| alternates. *)
  (* atoms *)
  "atom_recv", Emits [
      {|assign thread_0_wire\$[0-9]+ = _ep_req_valid;|};   (* m? reads valid ... *)
      {|assign thread_0_wire\$[0-9]+ = _ep_req_ack;|};     (* ... and ack ... *)
      {|thread_0_wire\$[0-9]+ && thread_0_wire\$[0-9]+;|}; (* ... and is their conjunction *)
      {|got: assert property (rst_ni && _thread_0_events\[[0-9]+\] |-> thread_0_wire\$[0-9]+);|} ]; (* one property form for every assertion *)
  "atom_send", Emits [
      {|assign thread_0_wire\$[0-9]+ = _ep_res_valid;|};
      {|assign thread_0_wire\$[0-9]+ = _ep_res_ack;|};
      {|sent: assert property (rst_ni && _thread_0_events\[[0-9]+\] |-> thread_0_wire\$[0-9]+);|} ];
  "atom_not_message", Fails "not a message";
  "atom_unknown_message", Fails "message";
  (* temporal operators: one concurrent property, event bit as antecedent *)
  "next", Emits [
      {|period: assert property (rst_ni && _thread_0_events\[[0-9]+\] |-> nexttime (nexttime (nexttime (thread_0_wire\$[0-9]+))));|} ];
  "always", Emits [
      {|hold: assert property (rst_ni && _thread_0_events\[[0-9]+\] |-> always (thread_0_wire\$[0-9]+));|} ];
  "eventually", Emits [
      {|answered: assert property (rst_ni && _thread_0_events\[[0-9]+\] |-> s_eventually (thread_0_wire\$[0-9]+));|} ];
  "temporal_outside_assert", Fails "Syntax error";   (* N outside an assert is not even a formula position *)
  (* regression *)
  "record_field", Emits [ {|module record_field|} ];
]

let config file : Anvil.Config.compile_config = {
  verbose = false; stdin = false; disable_lt_checks = false; weak_typecasts = true;   (* CLI defaults *)
  opt_level = 2; output_filename = None; just_check = false; two_round_graph = false;
  json_output = false; input_filenames = [file];
}

let error_text (msg : Anvil.Except.error_message) =
  List.filter_map (function Anvil.Except.Text t -> Some t | _ -> None) msg
  |> String.concat " "

(* Ok sv | Error message *)
let compile file =
  let tmp = Filename.temp_file "anvil_test" ".sv" in
  let out = open_out tmp in
  let result =
    try
      Anvil.CompileDriver.compile out (config file);
      close_out out;
      Ok (In_channel.with_open_text tmp In_channel.input_all)
    with
    | Anvil.CompileHelpers.CompileError msg -> close_out_noerr out; Error (error_text msg)
    | Anvil.Except.UnimplementedError msg -> close_out_noerr out; Error (error_text msg)
  in
  (try Sys.remove tmp with _ -> ());
  result

let contains_re sv re =
  try ignore (Str.search_forward (Str.regexp re) sv 0); true with Not_found -> false

let contains_sub s sub =
  try ignore (Str.search_forward (Str.regexp_string sub) s 0); true with Not_found -> false

let run (name, expect) =
  let file = Filename.concat "pipeline" (name ^ ".anvil") in
  let problems =
    match compile file, expect with
    | Ok sv, Emits res ->
      List.filter_map (fun re -> if contains_re sv re then None else Some ("missing: " ^ re)) res
    | Ok _, Fails sub -> ["compiled, expected failure containing: " ^ sub]
    | Error msg, Fails sub ->
      if contains_sub msg sub then [] else ["wrong error: " ^ msg]
    | Error msg, Emits _ -> ["did not compile: " ^ msg]
  in
  (match problems with
   | [] -> Printf.printf "PASS %s\n" name
   | ps -> Printf.printf "FAIL %s\n" name; List.iter (fun p -> Printf.printf "     %s\n" p) ps);
  problems = []

let () =
  let results = List.map run cases in
  let passed = List.length (List.filter Fun.id results) in
  Printf.printf "pipeline: %d/%d passed\n" passed (List.length results);
  if passed <> List.length results then exit 1
