(** Basic operations for event graphs. *)

(** Print the structure of the graph to standard error stream. *)
val print_graph : EventGraph.event_graph -> unit

(** Print the graph in dot format to output. *)
val print_dot_graph : EventGraph.event_graph -> out_channel -> unit

val lifetime_const : EventGraph.event -> EventGraph.lifetime
val lifetime_immediate : EventGraph.event -> EventGraph.lifetime

(** A {!EventGraph.subreg_range} that covers a full register. *)
val full_reg_range : Lang.identifier -> int -> EventGraph.subreg_range

(** Return [true] if we don't know for sure that two subreg ranges don't intersect. *)
val subreg_ranges_possibly_intersect : EventGraph.subreg_range -> EventGraph.subreg_range -> bool

(** Find the last event (recurse) in a given event graph. *)
val find_last_event : EventGraph.event_graph -> EventGraph.event

val string_of_actions : EventGraph.action -> string

val sub_reg_range : Lang.identifier -> int -> int -> EventGraph.subreg_range

(** A phrase naming the moment an event is reached, e.g. "the `recv ep.req` on line 9". *)
val describe_event : EventGraph.event -> string

(** A phrase naming when an event pattern is matched, e.g. "1 cycle after the `recv ep.req` on line 9".
    [None] for a pattern that never matches. *)
val describe_pat_end : EventGraph.event * Lang.delay_pat -> string option

(** A lifetime in words, e.g. "from the `recv ep.req` on line 9 until 1 cycle after the `recv ep.req` on line 9". *)
val describe_lifetime : EventGraph.lifetime -> string
