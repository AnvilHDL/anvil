(** Generate global states for a process. *)
val codegen_proc_states :
    CodegenPrinter.t ->
    EventGraph.proc_graph -> unit

(** Generate and print out code for states in a given design ({!EventGraph.event_graph}). *)
val codegen_states :
    CodegenPrinter.t ->
    EventGraph.event_graph_collection ->
    EventGraph.proc_graph ->
    Utils.StringSet.t ->
    EventGraph.event_graph ->
    Lang.message_specifier option -> unit

(** Registers assigned by multiple threads. *)
val shared_regs_of_proc : EventGraph.proc_graph -> Utils.StringSet.t

(** Single clocked block for the given registers. *)
val codegen_shared_reg_transitions :
    CodegenPrinter.t ->
    EventGraph.proc_graph -> Utils.StringSet.t -> unit
