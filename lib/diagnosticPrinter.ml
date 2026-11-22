(* Renders error and warning messages for the terminal. *)

type severity = Error | Warning

let severity_name = function
  | Error -> "error"
  | Warning -> "warning"

(* Spans longer than this show only their first and last two lines. *)
let max_full_lines = 4

let pad n = String.make n ' '

let span_of_fragment = function
  | Except.Codespan (_, span) | Except.Label (_, span, _) -> Some span
  | _ -> None

let has_source (span : Lang.code_span) =
  span.st.pos_fname <> "" && span.st.pos_lnum > 0

(* The source lines covered by a span, with their line numbers. *)
let read_span_lines (span : Lang.code_span) =
  try
    InChannelCacheableAliasable.with_open span.st.pos_fname (fun chn ->
      InChannelCacheableAliasable.seek chn (Int64.of_int span.st.pos_bol);
      let rec read lnum acc =
        if lnum > span.ed.pos_lnum then List.rev acc
        else match InChannelCacheableAliasable.input_line chn with
          | Some line -> read (lnum + 1) ((lnum, line) :: acc)
          | None -> List.rev acc
      in
      Some (read span.st.pos_lnum []))
  with _ -> None

(* Whitespace as wide as [line] up to [col], keeping tabs so carets line up. *)
let blank_prefix line col =
  String.init col (fun i -> if i < String.length line && line.[i] = '\t' then '\t' else ' ')

let first_non_blank line =
  let len = String.length line in
  let rec go i = if i < len && (line.[i] = ' ' || line.[i] = '\t') then go (i + 1) else i in
  go 0

let render_snippet buf width (span : Lang.code_span) label =
  let st = span.st and ed = span.ed in
  Printf.bprintf buf "%s--> %s:%d:%d\n" (pad width) st.pos_fname st.pos_lnum (st.pos_cnum - st.pos_bol + 1);
  match read_span_lines span with
  | None | Some [] -> ()
  | Some lines ->
    Printf.bprintf buf "%s |\n" (pad width);
    let n = List.length lines in
    let last_lnum = fst (List.nth lines (n - 1)) in
    let print_line (lnum, line) =
      let len = String.length line in
      let so = if lnum = st.pos_lnum then min (st.pos_cnum - st.pos_bol) len else first_non_blank line in
      let eo = if lnum = ed.pos_lnum then min (ed.pos_cnum - ed.pos_bol) len else len in
      let carets = if eo > so then eo - so else if n = 1 then 1 else 0 in
      let label_s = match label with
        | Some l when lnum = last_lnum -> " " ^ l
        | _ -> ""
      in
      Printf.bprintf buf "%*d | %s\n" width lnum line;
      if carets > 0 || label_s <> "" then
        Printf.bprintf buf "%s | %s%s%s\n" (pad width) (blank_prefix line so) (String.make carets '^') label_s
    in
    if n <= max_full_lines then
      List.iter print_line lines
    else (
      List.iteri (fun i l -> if i < 2 then print_line l) lines;
      Printf.bprintf buf "%s...\n" (pad width);
      List.iteri (fun i l -> if i >= n - 2 then print_line l) lines
    )

let render severity (msg : Except.error_message) =
  let buf = Buffer.create 512 in
  let width =
    List.filter_map span_of_fragment msg
    |> List.filter has_source
    |> List.fold_left (fun w (s : Lang.code_span) ->
        Int.max w (String.length (string_of_int s.ed.pos_lnum))) 1
  in
  let (headline, rest) = match msg with
    | Except.Text t :: rest -> (t, rest)
    | _ -> ("", msg)
  in
  if headline = "" then
    Printf.bprintf buf "%s\n" (severity_name severity)
  else
    Printf.bprintf buf "%s: %s\n" (severity_name severity) headline;
  List.iter (function
    | Except.Text t -> Printf.bprintf buf "note: %s\n" t
    | Except.Codespan (_, span) ->
      if has_source span then render_snippet buf width span None
    | Except.Label (_, span, label) ->
      if has_source span then render_snippet buf width span (Some label)
    | Except.Note t -> Printf.bprintf buf "%s = note: %s\n" (pad width) t
    | Except.Help t -> Printf.bprintf buf "%s = help: %s\n" (pad width) t
  ) rest;
  Buffer.contents buf

let print out severity msg =
  output_string out (render severity msg);
  output_string out "\n";
  flush out
