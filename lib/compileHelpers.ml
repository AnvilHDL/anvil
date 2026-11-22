exception CompileError of Except.error_message

let remap_missing_filename_in_fragment file_name fragments =
  let mapper = function
    | Except.Codespan (None, codespan) -> Except.Codespan (file_name, codespan)
    | Except.Label (None, codespan, label) -> Except.Label (file_name, codespan, label)
    | other -> other
  in
  match file_name with
  | Some _ -> List.map mapper fragments
  | None -> fragments

let issue_tracker_url = "https://github.com/AnvilHDL/anvil/issues"

(* Marks a message as describing a bug in the compiler. *)
let internal_error_message (msg : Except.error_message) =
  let open Except in
  let note = Note ("this is a bug in the Anvil compiler; please report it at " ^ issue_tracker_url) in
  match msg with
  | Text t :: rest -> (Text ("internal compiler error: " ^ t) :: rest) @ [note]
  | _ -> (Text "internal compiler error" :: msg) @ [note]

(* Describes an exception that escaped the compiler's own error handling. *)
let internal_error_of_exn (e : exn) =
  let text = match e with
    | Failure s -> s
    | Invalid_argument s -> "invalid argument: " ^ s
    | e -> Printexc.to_string e
  in
  internal_error_message [Except.Text text]

let convert_intermediate_anvil_errors (e: exn) file_name =
  let mapped = remap_missing_filename_in_fragment file_name in
  match e with
  | EventGraph.LifetimeCheckError msg
  | EventGraph.EventGraphError msg
  | Except.TypeError msg
  | Except.UnimplementedError msg
  | Except.CodegenError msg ->
      CompileError (mapped msg)
  | Except.InternalError msg ->
      CompileError (internal_error_message (mapped msg))
  | _ -> e

let raise_compile_error file_name msg =
  let msg = remap_missing_filename_in_fragment file_name msg in
  raise (CompileError msg)


let canonicalise_file_name file_origin file_name =
  if Filename.is_relative file_name then
    Filename.concat (Filename.dirname file_origin) file_name
  else
    file_name

let rec parse_recursive cunits parsed_files (config : Config.compile_config) filename =
  if Utils.StringSet.mem filename !parsed_files |> not then (
    parsed_files := Utils.StringSet.add filename !parsed_files;
    let cunit =
      let source_filename = match config.input_filenames with
        | top_level::_ when (config.stdin && top_level = filename) -> "-"
        | _ -> filename
      in
      try
        InChannelCacheableAliasable.with_open_aliased
          source_filename
          filename
          (fun in_data ->
            let lexbuf = Lexing.from_string (Bytes.to_string (in_data.buffer)) in
            lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename};
            let current_token () =
              Except.Codespan (Some filename, {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf})
            in
            match ParserDriver.parse lexbuf with
            | Ok cunit -> cunit
            | Error msg -> raise_compile_error (Some filename) msg
            | exception Lexer.SyntaxError msg -> raise_compile_error (Some filename) msg
            | exception Except.UnimplementedError msg -> raise_compile_error (Some filename) msg
            | exception ParserHelper.LiteralConversionError msg ->
              raise_compile_error (Some filename) (msg @ [current_token ()])
          )
      with
        | Sys_error msg ->
          raise_compile_error (Some filename) [Except.Text msg]
    in
    let cunit = {cunit with cunit_file_name = Some filename} in
    (
      try
        WellFormedCheck.syntax_tree_precheck config cunit
      with
        | Except.TypeError msg -> raise_compile_error (Some filename) msg
    );
    ErrorCollector.map_collected_errors (fun e ->
      convert_intermediate_anvil_errors e (Some filename)
    );
    cunits := (filename, cunit)::!cunits;
    List.iter (fun imp ->
      let open Lang in
      if not imp.is_extern then
        canonicalise_file_name filename imp.file_name
          |> parse_recursive cunits parsed_files config
    ) cunit.imports;

  )
