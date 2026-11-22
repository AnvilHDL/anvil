(* Incremental parser driver that builds syntax error messages. *)

module I = Parser.MenhirInterpreter

(* A sample token for each terminal. *)
let token_of_terminal : type a. a I.terminal -> Parser.token option = function
  | I.T_error -> None
  | I.T_EOF -> Some Parser.EOF
  | I.T_LEFT_BRACE -> Some Parser.LEFT_BRACE
  | I.T_RIGHT_BRACE -> Some Parser.RIGHT_BRACE
  | I.T_LEFT_BRACKET -> Some Parser.LEFT_BRACKET
  | I.T_RIGHT_BRACKET -> Some Parser.RIGHT_BRACKET
  | I.T_LEFT_PAREN -> Some Parser.LEFT_PAREN
  | I.T_RIGHT_PAREN -> Some Parser.RIGHT_PAREN
  | I.T_COMMA -> Some Parser.COMMA
  | I.T_SEMICOLON -> Some Parser.SEMICOLON
  | I.T_COLON -> Some Parser.COLON
  | I.T_DOUBLE_COLON -> Some Parser.DOUBLE_COLON
  | I.T_KEYWORD_FUNCTION -> Some Parser.KEYWORD_FUNCTION
  | I.T_KEYWORD_CALL -> Some Parser.KEYWORD_CALL
  | I.T_SHARP -> Some Parser.SHARP
  | I.T_EQUAL -> Some Parser.EQUAL
  | I.T_COLON_EQ -> Some Parser.COLON_EQ
  | I.T_LEFT_ABRACK -> Some Parser.LEFT_ABRACK
  | I.T_RIGHT_ABRACK -> Some Parser.RIGHT_ABRACK
  | I.T_LEFT_ABRACK_EQ -> Some Parser.LEFT_ABRACK_EQ
  | I.T_RIGHT_ABRACK_EQ -> Some Parser.RIGHT_ABRACK_EQ
  | I.T_SHL -> Some Parser.SHL
  | I.T_SHR -> Some Parser.SHR
  | I.T_KEYWORD_CONST -> Some Parser.KEYWORD_CONST
  | I.T_KEYWORD_READY -> Some Parser.KEYWORD_READY
  | I.T_KEYWORD_VOID -> Some Parser.KEYWORD_VOID
  | I.T_KEYWORD_PROBE -> Some Parser.KEYWORD_PROBE
  | I.T_DOUBLE_EQ -> Some Parser.DOUBLE_EQ
  | I.T_EQ_GT -> Some Parser.EQ_GT
  | I.T_DOUBLE_GT -> Some Parser.DOUBLE_GT
  | I.T_EXCL_EQ -> Some Parser.EXCL_EQ
  | I.T_ASTERISK -> Some Parser.ASTERISK
  | I.T_PLUS -> Some Parser.PLUS
  | I.T_MINUS -> Some Parser.MINUS
  | I.T_DOUBLE_MINUS -> Some Parser.DOUBLE_MINUS
  | I.T_XOR -> Some Parser.XOR
  | I.T_AND -> Some Parser.AND
  | I.T_DOUBLE_AND -> Some Parser.DOUBLE_AND
  | I.T_OR -> Some Parser.OR
  | I.T_DOUBLE_OR -> Some Parser.DOUBLE_OR
  | I.T_AT -> Some Parser.AT
  | I.T_TILDE -> Some Parser.TILDE
  | I.T_PERIOD -> Some Parser.PERIOD
  | I.T_KEYWORD_LOOP -> Some Parser.KEYWORD_LOOP
  | I.T_KEYWORD_PROC -> Some Parser.KEYWORD_PROC
  | I.T_KEYWORD_CHAN -> Some Parser.KEYWORD_CHAN
  | I.T_KEYWORD_LEFT -> Some Parser.KEYWORD_LEFT
  | I.T_KEYWORD_RIGHT -> Some Parser.KEYWORD_RIGHT
  | I.T_KEYWORD_LOGIC -> Some Parser.KEYWORD_LOGIC
  | I.T_KEYWORD_FOREIGN -> Some Parser.KEYWORD_FOREIGN
  | I.T_KEYWORD_GENERATE -> Some Parser.KEYWORD_GENERATE
  | I.T_KEYWORD_GENERATE_SEQ -> Some Parser.KEYWORD_GENERATE_SEQ
  | I.T_KEYWORD_IF -> Some Parser.KEYWORD_IF
  | I.T_KEYWORD_ELSE -> Some Parser.KEYWORD_ELSE
  | I.T_KEYWORD_LET -> Some Parser.KEYWORD_LET
  | I.T_KEYWORD_SEND -> Some Parser.KEYWORD_SEND
  | I.T_KEYWORD_RECV -> Some Parser.KEYWORD_RECV
  | I.T_KEYWORD_IN -> Some Parser.KEYWORD_IN
  | I.T_KEYWORD_ETERNAL -> Some Parser.KEYWORD_ETERNAL
  | I.T_KEYWORD_TYPE -> Some Parser.KEYWORD_TYPE
  | I.T_KEYWORD_SET -> Some Parser.KEYWORD_SET
  | I.T_KEYWORD_MATCH -> Some Parser.KEYWORD_MATCH
  | I.T_KEYWORD_DYN -> Some Parser.KEYWORD_DYN
  | I.T_KEYWORD_CYCLE -> Some Parser.KEYWORD_CYCLE
  | I.T_KEYWORD_REG -> Some Parser.KEYWORD_REG
  | I.T_KEYWORD_SPAWN -> Some Parser.KEYWORD_SPAWN
  | I.T_KEYWORD_DPRINT -> Some Parser.KEYWORD_DPRINT
  | I.T_KEYWORD_DFINISH -> Some Parser.KEYWORD_DFINISH
  | I.T_KEYWORD_IMPORT -> Some Parser.KEYWORD_IMPORT
  | I.T_KEYWORD_EXTERN -> Some Parser.KEYWORD_EXTERN
  | I.T_KEYWORD_INT -> Some Parser.KEYWORD_INT
  | I.T_KEYWORD_RECURSIVE -> Some Parser.KEYWORD_RECURSIVE
  | I.T_KEYWORD_RECURSE -> Some Parser.KEYWORD_RECURSE
  | I.T_KEYWORD_STRUCT -> Some Parser.KEYWORD_STRUCT
  | I.T_KEYWORD_ENUM -> Some Parser.KEYWORD_ENUM
  | I.T_KEYWORD_WITH -> Some Parser.KEYWORD_WITH
  | I.T_KEYWORD_TRY -> Some Parser.KEYWORD_TRY
  | I.T_INT -> Some (Parser.INT 0)
  | I.T_IDENT -> Some (Parser.IDENT "")
  | I.T_BIT_LITERAL -> Some (Parser.BIT_LITERAL "")
  | I.T_DEC_LITERAL -> Some (Parser.DEC_LITERAL "")
  | I.T_HEX_LITERAL -> Some (Parser.HEX_LITERAL "")
  | I.T_STR_LITERAL -> Some (Parser.STR_LITERAL "")

(* Source spelling of a token without a payload. *)
let spelling : Parser.token -> string option = function
  | Parser.LEFT_BRACE -> Some "{"
  | Parser.RIGHT_BRACE -> Some "}"
  | Parser.LEFT_BRACKET -> Some "["
  | Parser.RIGHT_BRACKET -> Some "]"
  | Parser.LEFT_PAREN -> Some "("
  | Parser.RIGHT_PAREN -> Some ")"
  | Parser.COMMA -> Some ","
  | Parser.SEMICOLON -> Some ";"
  | Parser.COLON -> Some ":"
  | Parser.DOUBLE_COLON -> Some "::"
  | Parser.KEYWORD_FUNCTION -> Some "func"
  | Parser.KEYWORD_CALL -> Some "call"
  | Parser.SHARP -> Some "#"
  | Parser.EQUAL -> Some "="
  | Parser.COLON_EQ -> Some ":="
  | Parser.LEFT_ABRACK -> Some "<"
  | Parser.RIGHT_ABRACK -> Some ">"
  | Parser.LEFT_ABRACK_EQ -> Some "<="
  | Parser.RIGHT_ABRACK_EQ -> Some ">="
  | Parser.SHL -> Some "shl"
  | Parser.SHR -> Some "shr"
  | Parser.KEYWORD_CONST -> Some "const"
  | Parser.KEYWORD_READY -> Some "ready"
  | Parser.KEYWORD_VOID -> Some "void"
  | Parser.KEYWORD_PROBE -> Some "probe"
  | Parser.DOUBLE_EQ -> Some "=="
  | Parser.EQ_GT -> Some "=>"
  | Parser.DOUBLE_GT -> Some ">>"
  | Parser.EXCL_EQ -> Some "!="
  | Parser.ASTERISK -> Some "*"
  | Parser.PLUS -> Some "+"
  | Parser.MINUS -> Some "-"
  | Parser.DOUBLE_MINUS -> Some "--"
  | Parser.XOR -> Some "^"
  | Parser.AND -> Some "&"
  | Parser.DOUBLE_AND -> Some "&&"
  | Parser.OR -> Some "|"
  | Parser.DOUBLE_OR -> Some "||"
  | Parser.AT -> Some "@"
  | Parser.TILDE -> Some "~"
  | Parser.PERIOD -> Some "."
  | Parser.KEYWORD_LOOP -> Some "loop"
  | Parser.KEYWORD_PROC -> Some "proc"
  | Parser.KEYWORD_CHAN -> Some "chan"
  | Parser.KEYWORD_LEFT -> Some "left"
  | Parser.KEYWORD_RIGHT -> Some "right"
  | Parser.KEYWORD_LOGIC -> Some "logic"
  | Parser.KEYWORD_FOREIGN -> Some "foreign"
  | Parser.KEYWORD_GENERATE -> Some "generate"
  | Parser.KEYWORD_GENERATE_SEQ -> Some "generate_seq"
  | Parser.KEYWORD_IF -> Some "if"
  | Parser.KEYWORD_ELSE -> Some "else"
  | Parser.KEYWORD_LET -> Some "let"
  | Parser.KEYWORD_SEND -> Some "send"
  | Parser.KEYWORD_RECV -> Some "recv"
  | Parser.KEYWORD_IN -> Some "in"
  | Parser.KEYWORD_ETERNAL -> Some "eternal"
  | Parser.KEYWORD_TYPE -> Some "type"
  | Parser.KEYWORD_SET -> Some "set"
  | Parser.KEYWORD_MATCH -> Some "match"
  | Parser.KEYWORD_DYN -> Some "dyn"
  | Parser.KEYWORD_CYCLE -> Some "cycle"
  | Parser.KEYWORD_REG -> Some "reg"
  | Parser.KEYWORD_SPAWN -> Some "spawn"
  | Parser.KEYWORD_DPRINT -> Some "dprint"
  | Parser.KEYWORD_DFINISH -> Some "dfinish"
  | Parser.KEYWORD_IMPORT -> Some "import"
  | Parser.KEYWORD_EXTERN -> Some "extern"
  | Parser.KEYWORD_INT -> Some "int"
  | Parser.KEYWORD_RECURSIVE -> Some "recursive"
  | Parser.KEYWORD_RECURSE -> Some "recurse"
  | Parser.KEYWORD_STRUCT -> Some "struct"
  | Parser.KEYWORD_ENUM -> Some "enum"
  | Parser.KEYWORD_WITH -> Some "with"
  | Parser.KEYWORD_TRY -> Some "try"
  | Parser.INT _ | Parser.IDENT _ | Parser.BIT_LITERAL _ | Parser.DEC_LITERAL _ | Parser.HEX_LITERAL _ | Parser.STR_LITERAL _ | Parser.EOF -> None

let is_word s =
  s <> "" && (match s.[0] with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false)

(* Description of a found token, e.g. "keyword `loop`". *)
let describe_found (tok : Parser.token) =
  match tok with
  | Parser.EOF -> "end of file"
  | Parser.IDENT s -> Printf.sprintf "identifier `%s`" s
  | Parser.INT n -> Printf.sprintf "integer `%d`" n
  | Parser.BIT_LITERAL s | Parser.DEC_LITERAL s | Parser.HEX_LITERAL s -> Printf.sprintf "literal `%s`" s
  | Parser.STR_LITERAL s -> Printf.sprintf "string \"%s\"" s
  | _ ->
    match spelling tok with
    | Some s when is_word s -> Printf.sprintf "keyword `%s`" s
    | Some s -> Printf.sprintf "`%s`" s
    | None -> "token"

(* Description of an expected token, e.g. "an identifier". *)
let describe_expected (tok : Parser.token) =
  match tok with
  | Parser.EOF -> "end of file"
  | Parser.IDENT _ -> "an identifier"
  | Parser.INT _ -> "an integer"
  | Parser.BIT_LITERAL _ | Parser.DEC_LITERAL _ | Parser.HEX_LITERAL _ -> "a literal"
  | Parser.STR_LITERAL _ -> "a string"
  | _ ->
    match spelling tok with
    | Some s -> Printf.sprintf "`%s`" s
    | None -> "token"

(* Maximum number of expected tokens listed. *)
let max_expected = 6

(* Binary and postfix operators. *)
let is_operator (tok : Parser.token) =
  match tok with
  | Parser.PLUS | Parser.MINUS | Parser.ASTERISK | Parser.XOR | Parser.AND | Parser.OR
  | Parser.DOUBLE_AND | Parser.DOUBLE_OR | Parser.DOUBLE_EQ | Parser.EXCL_EQ
  | Parser.LEFT_ABRACK | Parser.RIGHT_ABRACK | Parser.LEFT_ABRACK_EQ | Parser.RIGHT_ABRACK_EQ
  | Parser.SHL | Parser.SHR | Parser.KEYWORD_IN | Parser.PERIOD | Parser.LEFT_BRACKET -> true
  | _ -> false

let dedup l = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l |> List.rev

(* Descriptions of the tokens accepted at [checkpoint]. *)
let expected_tokens checkpoint pos =
  let accepted =
    I.foreach_terminal_but_error (fun (I.X sym) acc ->
      match sym with
      | I.T t ->
        (match token_of_terminal t with
         | Some tok when I.acceptable checkpoint tok pos -> (tok, I.first I.N_expr t) :: acc
         | _ -> acc)
      | I.N _ -> acc
    ) []
    |> List.rev
  in
  let count p l = List.length (List.filter p l) in
  let accepts_ident = List.exists (fun (tok, _) -> match tok with Parser.IDENT _ -> true | _ -> false) accepted in
  let group_expr = accepts_ident && count (fun (tok, starts) -> starts && not (is_operator tok)) accepted >= 5 in
  let rest = if group_expr then List.filter (fun (_, starts) -> not starts) accepted else accepted in
  let group_ops = count (fun (tok, _) -> is_operator tok) rest >= 3 in
  let singles = if group_ops then List.filter (fun (tok, _) -> not (is_operator tok)) rest else rest in
  dedup (List.map (fun (tok, _) -> describe_expected tok) singles)
  @ (if group_expr then ["an expression"] else [])
  @ (if group_ops then ["an operator"] else [])

(* Whether [found] can start an expression. *)
let starts_expression (found : Parser.token) =
  let d = describe_expected found in
  I.foreach_terminal_but_error (fun (I.X sym) acc ->
    acc || match sym with
      | I.T t ->
        (match token_of_terminal t with
         | Some tok -> describe_expected tok = d && I.first I.N_expr t
         | None -> false)
      | I.N _ -> false
  ) false

let format_expected = function
  | [] -> None
  | [a] -> Some a
  | [a; b] -> Some (a ^ " or " ^ b)
  | l when List.length l <= max_expected -> Some ("one of " ^ String.concat ", " l)
  | _ -> None

(* Hand-written message for a parser state, if any. *)
let custom_message state =
  match ParserMessages.message state with
  | exception Not_found -> None
  | text ->
    let lines = String.split_on_char '\n' text |> List.map String.trim |> List.filter ((<>) "") in
    let fragment line =
      let after prefix = String.sub line (String.length prefix) (String.length line - String.length prefix) in
      if String.starts_with ~prefix:"help: " line then Except.Help (after "help: ")
      else if String.starts_with ~prefix:"note: " line then Except.Note (after "note: ")
      else Except.Note line
    in
    match lines with
    | [] -> None
    | headline :: rest -> Some (headline, List.map fragment rest)

let syntax_error state expected (found : Parser.token) (span : Lang.code_span) : Except.error_message =
  let found_s = "unexpected " ^ describe_found found in
  let expected_s = Option.map (fun e -> "expected " ^ e) (format_expected expected) in
  let state_note = match Sys.getenv_opt "ANVIL_PARSER_STATE" with
    | Some _ -> [Except.Note (Printf.sprintf "parser state %d" state)]
    | None -> []
  in
  let msg = match custom_message state with
    | Some (headline, extra) ->
      [Except.Text headline; Except.Label (None, span, found_s)] @ extra
    | None ->
      let unclosed =
        if found = Parser.EOF && List.mem "`}`" expected then [Except.Help "is a closing `}` missing?"] else []
      in
      let missing_separator =
        if List.mem "`>>`" expected && not (List.mem "`{`" expected) && starts_expression found then
          [Except.Help "put `>>` between the two expressions to run one after the other, or `;` to run them at the same time"]
        else []
      in
      (match expected_s with
       | Some e -> [Except.Text found_s; Except.Label (None, span, e)]
       | None -> [Except.Text found_s; Except.Codespan (None, span)])
      @ unclosed @ missing_separator
  in
  msg @ state_note

let parse (lexbuf : Lexing.lexbuf) : (Lang.compilation_unit, Except.error_message) result =
  let rec run last_input last_token prev_end checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
      let prev_end = match last_token with Some (_, _, ed) -> ed | None -> prev_end in
      let tok = Lexer.read lexbuf in
      let triple = (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
      run (Some checkpoint) (Some triple) prev_end (I.offer checkpoint triple)
    | I.Shifting _ | I.AboutToReduce _ ->
      run last_input last_token prev_end (I.resume checkpoint)
    | I.HandlingError env ->
      let state = I.current_state_number env in
      (match last_token, last_input with
       | Some (tok, st, ed), Some input ->
         let span = if tok = Parser.EOF then {Lang.st = prev_end; ed = prev_end} else {Lang.st; ed} in
         Error (syntax_error state (expected_tokens input st) tok span)
       | _ -> Error [Except.Text "syntax error"])
    | I.Accepted cunit -> Ok cunit
    | I.Rejected -> Error [Except.Text "syntax error"]
  in
  let start = lexbuf.Lexing.lex_curr_p in
  run None None start (Parser.Incremental.cunit start)
