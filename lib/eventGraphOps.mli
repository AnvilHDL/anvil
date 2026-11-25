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

val string_of_actions : EventGraph.action -> string

val sub_reg_range : Lang.identifier -> int -> int -> EventGraph.subreg_range

(** A phrase naming the moment an event is reached, e.g. "the `recv ep.req` on line 9". *)
val describe_event : EventGraph.event -> string

(** When a delay pattern ends after the moment named [at], e.g. "1 cycle after [at]", or "forever". *)
val describe_pat_after : Lang.delay_pat -> string -> string

(** "1 cycle", "3 cycles". *)
val cycles_phrase : int -> string
