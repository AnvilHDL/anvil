(** Renders error and warning messages for the terminal. *)

type severity = Error | Warning

(** Renders a message: a headline from its first [Text], then code snippets,
    notes and help lines. *)
val render : severity -> Except.error_message -> string

(** Prints a rendered message followed by a blank line. *)
val print : out_channel -> severity -> Except.error_message -> unit
