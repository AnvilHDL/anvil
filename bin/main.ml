let json_warnings () =
  Anvil.Except.take_warnings ()
  |> List.map (Anvil.JsonOutput.error_message_to_json_error "warning")

let compile_with_json_output config =
  let open Anvil.Config in
  if config.input_filenames = [] then begin
  let json_errors = [Anvil.JsonOutput.{
    error_type = "error";
    path = None;
    description = [ { kind = "text"; text = Some "Error: a file name must be supplied!"; trace = None; label = None } ]
  }] in
    let json_result = Anvil.JsonOutput.failure_output json_errors in
    print_endline (Anvil.JsonOutput.json_output_to_string json_result);
    exit 1
  end else begin
    let temp_file = Filename.temp_file "anvil_output" ".sv" in
    let fail msg =
      (try Sys.remove temp_file with _ -> ());
      let json_error = Anvil.JsonOutput.error_message_to_json_error "error" msg in
      let json_result = Anvil.JsonOutput.failure_output (json_error :: json_warnings ()) in
      print_endline (Anvil.JsonOutput.json_output_to_string json_result);
      exit 1
    in
    try
      Anvil.AstAnnotator.enabled := config.ast_output;
      if config.ast_output then
        let parse_result = Anvil.CompileDriver.parse config in
        let json_errors =
          let convert error_msg = Anvil.JsonOutput.error_message_to_json_error "error" error_msg in
          let compile_error_to_json_error (e: exn) =
            match e with
            | Anvil.CompileHelpers.CompileError msg -> convert msg
            | e -> convert (Anvil.CompileHelpers.internal_error_of_exn e)
           in
          List.map compile_error_to_json_error parse_result.errors
        in
        let json_result = Anvil.JsonOutput.ast_output
          parse_result.compilation_units parse_result.graph_collections json_errors in
        let json_result = Anvil.JsonOutput.{ json_result with errors = json_result.errors @ json_warnings () } in
        print_endline (Anvil.JsonOutput.json_output_to_string json_result);
        exit 0
      else ();

      let temp_out = open_out temp_file in
      (try
        Anvil.CompileDriver.compile temp_out config;
        close_out temp_out;
        let output_content = In_channel.with_open_text temp_file In_channel.input_all in
        let json_result = Anvil.JsonOutput.transpiled_output output_content in
        let json_result = Anvil.JsonOutput.{ json_result with errors = json_warnings () } in
        print_endline (Anvil.JsonOutput.json_output_to_string json_result)
      with
        | exn ->
          close_out_noerr temp_out;
          raise exn
      );
      Sys.remove temp_file
    with
    | Anvil.CompileHelpers.CompileError msg -> fail msg
    | e -> fail (Anvil.CompileHelpers.internal_error_of_exn e)
  end

let print_warnings () =
  Anvil.Except.take_warnings ()
  |> List.iter (Anvil.DiagnosticPrinter.print stderr Anvil.DiagnosticPrinter.Warning)

let compile_with_normal_output config =
  let open Anvil.Config in
  if config.input_filenames = [] && not config.stdin then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    let out_channel = match config.output_filename with
      | Some filename -> open_out (Printf.sprintf "%s.anvil.sv" filename)
      | None -> stdout
    in
    let fail msg =
      if Option.is_some config.output_filename then
        close_out_noerr out_channel;
      print_warnings ();
      Anvil.DiagnosticPrinter.print stderr Anvil.DiagnosticPrinter.Error msg;
      exit 1
    in
    try
      Anvil.CompileDriver.compile out_channel config;
      if Option.is_some config.output_filename then
        close_out out_channel;
      print_warnings ()
    with
    | Anvil.CompileHelpers.CompileError msg -> fail msg
    | e -> fail (Anvil.CompileHelpers.internal_error_of_exn e)
  end

let () =
  let config = Anvil.Config.parse_args() in
  if config.ast_output && not config.json_output then begin
    Printf.eprintf "Error: -ast requires -json to be set!\n";
    exit 1
  end else if config.json_output then
    compile_with_json_output config
  else
    compile_with_normal_output config
