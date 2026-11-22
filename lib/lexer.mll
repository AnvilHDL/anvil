{
  open Parser
  exception SyntaxError of Except.error_message

  let error ?(extra = []) text st ed =
    raise (SyntaxError (Except.Text text :: Except.Codespan (None, { Lang.st; ed }) :: extra))

  let advance (p : Lexing.position) n = { p with pos_cnum = p.pos_cnum + n }

  (* Notes on why a malformed literal is invalid. *)
  let literal_notes s =
    let i = String.index s '\'' in
    let len = String.length s in
    let digits = if len > i + 2 then String.sub s (i + 2) (len - i - 2) else "" in
    let format_help =
      Except.Help "literals are written as <width>'<base><digits>, e.g. `8'd255`, `4'b1010` or `8'hff`"
    in
    if len = i + 1 then [format_help]
    else match s.[i + 1] with
      | 'b' | 'd' | 'h' when digits = "" ->
        [Except.Note (Printf.sprintf "no digits after `%s`" (String.sub s 0 (i + 2))); format_help]
      | 'b' -> [Except.Note "binary literals can only contain the digits 0 and 1"]
      | 'd' -> [Except.Note "decimal literals can only contain the digits 0 to 9"]
      | 'h' -> [Except.Note "hexadecimal literals can only contain the digits 0 to 9 and a to f"]
      | c ->
        [Except.Note (Printf.sprintf "unknown base `%c`; the base is `b` (binary), `d` (decimal) or `h` (hexadecimal)" c);
         format_help]
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
(* only non-negatives *)
let int = ['0'-'9']+
let bit_literal = int "\'b" ['0'-'1']+
let dec_literal = int "\'d" ['0'-'9']+
let hex_literal = int "\'h" ['0'-'9' 'a'-'f' 'A'-'F']+
(* a literal-like token that is not a valid literal *)
let malformed_literal = int '\'' ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let utf8_char = ['\xc0'-'\xff'] ['\x80'-'\xbf']*

rule read =
  parse
  | white     { read lexbuf }
  | newline   { Lexing.new_line lexbuf; read lexbuf }
  | '{'       { LEFT_BRACE }
  | '}'       { RIGHT_BRACE }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ','       { COMMA }
  | ':'       { COLON }
  | ';'       { SEMICOLON }
  | '#'       { SHARP }
  | '*'       { ASTERISK }
  | '='       { EQUAL }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | "--"      { DOUBLE_MINUS }
  | '^'       { XOR }
  | "&&"      { DOUBLE_AND }
  | '&'       { AND }
  | "||"      { DOUBLE_OR }
  | '|'       { OR }
  | '@'       { AT }
  | '~'       { TILDE }
  | '.'       { PERIOD }
  | ":="      { COLON_EQ }
  | "::"      { DOUBLE_COLON }
  | "<"       { LEFT_ABRACK }
  | ">"       { RIGHT_ABRACK }
  | "<<"      {
      error ~extra:[Except.Help "shift left is written `shl`, e.g. `x shl 2`"]
        "`<<` is not an operator" (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)
    }
  | "<="      { LEFT_ABRACK_EQ }
  | ">="      { RIGHT_ABRACK_EQ }
  | "=>"      { EQ_GT }
  | ">>"      { DOUBLE_GT }
  | "=="      { DOUBLE_EQ }
  | "!="      { EXCL_EQ }
  | "ready"   { KEYWORD_READY }
  | "probe"   { KEYWORD_PROBE }
  | "void"    { KEYWORD_VOID }
  | "func"    { KEYWORD_FUNCTION }
  | "in"      { KEYWORD_IN }
  | "call"    { KEYWORD_CALL }
  | "loop"    { KEYWORD_LOOP }
  | "proc"    { KEYWORD_PROC }
  | "chan"    { KEYWORD_CHAN }
  | "left"    { KEYWORD_LEFT }
  | "right"   { KEYWORD_RIGHT }
  | "logic"   { KEYWORD_LOGIC }
  | "foreign" { KEYWORD_FOREIGN }
  | "shl"     { SHL }
  | "shr"     { SHR }
  | "let"     { KEYWORD_LET }
  | "if"      { KEYWORD_IF }
  | "else"    { KEYWORD_ELSE }
  | "send"    { KEYWORD_SEND }
  | "recv"    { KEYWORD_RECV }
  | "eternal" { KEYWORD_ETERNAL }
  | "type"    { KEYWORD_TYPE }
  | "set"     { KEYWORD_SET }
  | "match"   { KEYWORD_MATCH }
  | "dyn"     { KEYWORD_DYN }
  | "cycle"   { KEYWORD_CYCLE }
  | "reg"     { KEYWORD_REG }
  | "const"   { KEYWORD_CONST }
  | "spawn"   { KEYWORD_SPAWN }
  | "dprint"  { KEYWORD_DPRINT }
  | "dfinish" { KEYWORD_DFINISH }
  | "import"  { KEYWORD_IMPORT }
  | "extern"  { KEYWORD_EXTERN }
  | "int"     { KEYWORD_INT }
  | "generate" { KEYWORD_GENERATE }
  | "generate_seq" { KEYWORD_GENERATE_SEQ }
  | "recursive" { KEYWORD_RECURSIVE }
  | "recurse" { KEYWORD_RECURSE }
  | "struct"  { KEYWORD_STRUCT }
  | "enum"    { KEYWORD_ENUM }
  | "with"    { KEYWORD_WITH }
  | "try"     { KEYWORD_TRY }
  | int       { let n = Lexing.lexeme lexbuf |> int_of_string in INT n }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | bit_literal { BIT_LITERAL (Lexing.lexeme lexbuf) }
  | dec_literal { DEC_LITERAL (Lexing.lexeme lexbuf) }
  | hex_literal { HEX_LITERAL (Lexing.lexeme lexbuf) }
  | malformed_literal {
      let s = Lexing.lexeme lexbuf in
      error ~extra:(literal_notes s) (Printf.sprintf "invalid literal `%s`" s)
        (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)
    }
  | eof       { EOF }
  | "/*"      { skip_comments (Lexing.lexeme_start_p lexbuf) lexbuf }
  | "//"      { skip_inline_comments lexbuf }
  | '"'       {
      let start = Lexing.lexeme_start_p lexbuf in
      let tok = read_string (Buffer.create 32) start lexbuf in
      lexbuf.lex_start_p <- start;
      tok
    }
  | utf8_char | _ {
      error (Printf.sprintf "unknown character `%s`" (Lexing.lexeme lexbuf))
        (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)
    }
and skip_comments start =
  parse
  | newline   { Lexing.new_line lexbuf; skip_comments start lexbuf }
  | "*/"      { read lexbuf }
  | _         { skip_comments start lexbuf }
  | eof       {
      error ~extra:[Except.Help "close the comment with `*/`"]
        "unterminated comment" start (advance start 2)
    }
and skip_inline_comments =
  parse
  | newline   { Lexing.new_line lexbuf; read lexbuf }
  | _         { skip_inline_comments lexbuf }
  | eof       { EOF }
and read_string buf start =
  parse
  | newline | eof {
      error ~extra:[Except.Help "strings must end on the line they start; close it with `\"`"]
        "unterminated string" start (Lexing.lexeme_start_p lexbuf)
    }
  | '"'     { STR_LITERAL (Buffer.contents buf) }
  | "\\\""  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf start lexbuf }
  | _       { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf start lexbuf }
