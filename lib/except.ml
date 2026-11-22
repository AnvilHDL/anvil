(** A fragment in an error message. The first [Text] is the headline. *)
type error_message_fragment =
| Text of string (** text *)
| Codespan of string option * Lang.code_span (** [(filename, span)],
                                                 [filename] is optional *)
| Label of string option * Lang.code_span * string (** a code span with a short label *)
| Note of string (** extra information *)
| Help of string (** a suggested fix *)

(** A helper function for obtaining a {!Codespan} without a file name. *)
let codespan_local span = Codespan (None, span)

(** A helper function for obtaining a {!Codespan} with a specified file name. *)
let codespan_in filename span = Codespan (Some filename, span)

(** A helper function for obtaining a {!Codespan} using the file name embedded
    in the span's [Lexing.position]. *)
let codespan_auto span =
  let f = span.Lang.st.Lexing.pos_fname in
  Codespan ((if f = "" then None else Some f), span)

type error_message = error_message_fragment list

exception UnimplementedError of error_message
exception TypeError of error_message
(* A bug in the compiler rather than in the user's design. *)
exception InternalError of error_message
exception CodegenError of error_message


let internal_error msg_text = InternalError [Text msg_text]

(* Warnings collected during compilation, most recent first. *)
let warnings : error_message list ref = ref []

(** Records a warning; a warning identical to an earlier one is dropped. *)
let warn (msg : error_message) =
  if not (List.mem msg !warnings) then
    warnings := msg :: !warnings

(** Returns the recorded warnings in order and clears them. *)
let take_warnings () =
  let w = List.rev !warnings in
  warnings := [];
  w
