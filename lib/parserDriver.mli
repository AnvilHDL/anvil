(** Runs the parser and turns syntax errors into error messages. *)

(** Parses a compilation unit, returning a syntax error as an error message. *)
val parse : Lexing.lexbuf -> (Lang.compilation_unit, Except.error_message) result
