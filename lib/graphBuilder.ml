open EventGraph
open EventGraphOps
open Lang
open ErrorCollector
open GraphBuildContext
open DataTypeCheck

type build_context = Typing.build_context
module BuildContext = Typing.BuildContext

let unwrap_or_err err_msg err_span opt =
  match opt with
  | Some d -> d
  | None -> raise_fatal (event_graph_error_default err_msg err_span)

(* Records what reaches [ev] in the source, unless it is already known. *)
let set_origin (ev : event) what span =
  if Option.is_none ev.origin then ev.origin <- Some {what; where = span}

(* Warning for reading part of [reg] at a position known only at run time. *)
let dynamic_index_warning reg span =
  Except.warn [
    Text (Printf.sprintf "`%s` is read at a position known only at run time" reg);
    Label (None, span, "the index is not a constant");
    Note (Printf.sprintf "the compiler cannot tell which part of `%s` is read, so all of `%s` counts as in use while the value is needed" reg reg)
  ]

let register_names (graph : event_graph) =
  Utils.StringMap.bindings graph.regs |> List.map (fun (name, _) -> (name, "register"))

let value_names (ci : cunit_info) (ctx : build_context) =
  (Utils.StringMap.bindings ctx.cg_lt_ctx |> List.map (fun (name, _) -> (name, "value")))
  @ List.map (fun (m : macro_def) -> (m.id, "constant")) ci.macro_defs

(* Error for reading or assigning [ident] as a register when it is not one. *)
let not_a_register (graph : event_graph) (ci : cunit_info) (ctx : build_context) ~reading ident span =
  let open Except in
  let action = if reading then "`*` reads a register" else "`set` assigns to a register" in
  let msg = match Typing.context_lookup ctx.cg_lt_ctx ident with
    | Some binding ->
      [
        Text (Printf.sprintf "`%s` is not a register" ident);
        Label (None, span, Printf.sprintf "%s, but `%s` is a value" action ident);
        Label (None, binding.binding_def_span, Printf.sprintf "`%s` is bound here" ident);
        if reading then
          Help (Printf.sprintf "use the value directly by writing `%s` instead of `*%s`" ident ident)
        else
          Help "a value cannot change once it is bound; declare a register with `reg` to hold state that changes"
      ]
    | None when List.exists (fun (m : macro_def) -> m.id = ident) ci.macro_defs ->
      [
        Text (Printf.sprintf "`%s` is not a register" ident);
        Label (None, span, Printf.sprintf "%s, but `%s` is a constant" action ident);
      ]
      @ (if reading then [Help (Printf.sprintf "use the constant directly by writing `%s` instead of `*%s`" ident ident)] else [])
    | None ->
      Text (Printf.sprintf "no register named `%s`" ident) :: codespan_local span
      :: did_you_mean ident (register_names graph)
  in
  raise_fatal (EventGraphError msg)

(* Error for an identifier that names no value in scope. *)
let unknown_value (graph : event_graph) (ci : cunit_info) (ctx : build_context) ident span =
  let open Except in
  let msg =
    if Utils.StringMap.mem ident graph.regs then
      [
        Text (Printf.sprintf "register `%s` is read without `*`" ident);
        Label (None, span, "registers are read with `*`");
        Help (Printf.sprintf "write `*%s` to read the current value of the register" ident)
      ]
    else
      Text (Printf.sprintf "no value named `%s` in this scope" ident) :: codespan_local span
      :: did_you_mean ident (value_names ci ctx @ register_names graph)
  in
  raise_fatal (EventGraphError msg)

let side_name = function Left -> "left" | Right -> "right"

(* Joins names as "`a`, `b` and `c`". *)
let code_list names =
  let quoted = List.map (fun n -> "`" ^ n ^ "`") names in
  match List.rev quoted with
  | [] -> ""
  | [n] -> n
  | last :: rest -> String.concat ", " (List.rev rest) ^ " and " ^ last

(* The definition of the message [msg_spec] as seen from this process. *)
let message_of (graph : event_graph) (ci : cunit_info) (msg_spec : message_specifier) span =
  let open Except in
  let fail msg = raise_fatal (EventGraphError msg) in
  let ep_name = msg_spec.endpoint in
  match MessageCollection.lookup_endpoint graph.messages ep_name with
  | None ->
    let endpoints = List.map (fun (ep : endpoint_def) -> (ep.name, "endpoint"))
        (graph.messages.endpoints @ graph.messages.args) in
    fail (Text (Printf.sprintf "no endpoint named `%s`" ep_name) :: codespan_local span
          :: did_you_mean ep_name endpoints)
  | Some ep when ep.foreign ->
    let spawn_label = List.find_opt (fun (sp : spawn_def ast_node) ->
        List.mem ep.name (Lang.preprocess_ep_spawn_args sp.d.params)) graph.spawns
      |> Option.map (fun (sp : spawn_def ast_node) ->
        Label (None, sp.span, Printf.sprintf "`%s` is passed to `%s` here" ep_name sp.d.proc))
      |> Option.to_list
    in
    fail ([
      Text (Printf.sprintf "endpoint `%s` cannot be used in this process" ep_name);
      Label (None, span, Printf.sprintf "`%s` is used here" ep_name)
    ] @ spawn_label @ [Note "an endpoint passed to `spawn` belongs to the spawned process"])
  | Some ep ->
    let cc = MessageCollection.lookup_channel_class ci.channel_classes ep.channel_class in
    match cc, MessageCollection.lookup_message graph.messages msg_spec ci.channel_classes with
    | Some cc, None when not (List.exists (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages) ->
      let names = List.map (fun (m : message_def) -> m.name) cc.messages in
      let hint = match did_you_mean msg_spec.msg (List.map (fun n -> (n, "message")) names) with
        | [] when names = [] -> [Note (Printf.sprintf "`%s` has no messages" cc.name)]
        | [] -> [Note (Printf.sprintf "the messages of `%s` are %s" cc.name (code_list names))]
        | h -> h
      in
      fail (Text (Printf.sprintf "channel class `%s` has no message `%s`" cc.name msg_spec.msg)
            :: Label (None, span, Printf.sprintf "`%s` is an endpoint of `%s`" ep_name cc.name)
            :: hint)
    | _, Some msg -> msg
    | _ -> fail [Text (Printf.sprintf "invalid message `%s.%s`" ep_name msg_spec.msg); codespan_local span]

(* The number of cycles in a `cycle`, resolved from a parameter or constant. *)
let cycle_count (ci : cunit_info) (ctx : build_context) (n : int ParamEnv.maybe_param) span =
  match n with
  | ParamEnv.Concrete v -> v
  | ParamEnv.Param p ->
    match List.find_opt (fun (m : macro_def) -> m.id = p) ci.macro_defs with
    | Some m -> m.value
    | None ->
      let open Except in
      let msg = match Typing.context_lookup ctx.cg_lt_ctx p with
        | Some binding ->
          [
            Text "the number of cycles must be known at compile time";
            Label (None, span, Printf.sprintf "`%s` is a value computed at run time" p);
            Label (None, binding.binding_def_span, Printf.sprintf "`%s` is bound here" p);
            Note "`cycle` takes an integer literal, a parameter or a constant, e.g. `cycle 2` or `cycle N`"
          ]
        | None ->
          Text (Printf.sprintf "no parameter or constant named `%s`" p) :: codespan_local span
          :: did_you_mean p (List.map (fun (m : macro_def) -> (m.id, "constant")) ci.macro_defs)
      in
      raise_fatal (EventGraphError msg)

(* Error for indexing a value of type [dtype] with [idx]. *)
let bad_index (ci : cunit_info) (dtype : data_type) (idx : index) span =
  let open Except in
  let msg = match TypedefMap.data_type_name_resolve ci.typedefs dtype, idx with
    | Some (`Array _), Range _ ->
      [
        Text "the size of a range index must be an integer literal";
        codespan_local span;
        Help "a range is written as `start +: size`, e.g. `a[i +: 4]`"
      ]
    | _ ->
      [
        Text (Printf.sprintf "cannot index a value of type `%s`" (string_of_data_type dtype));
        codespan_local span;
        Note "only arrays can be indexed"
      ]
  in
  raise_fatal (EventGraphError msg)

(* Error for accessing [field] of a value of type [dtype] that has no such field. *)
let bad_field (ci : cunit_info) (dtype : data_type) field span =
  let open Except in
  let type_s = string_of_data_type dtype in
  let msg = match TypedefMap.data_type_name_resolve ci.typedefs dtype with
    | Some (`Record fields) ->
      let names = List.map (fun ({d = (n, _); _} : (identifier * data_type) ast_node) -> n) fields in
      let hint = match did_you_mean field (List.map (fun n -> (n, "field")) names) with
        | [] -> [Note (Printf.sprintf "the fields of `%s` are %s" type_s (code_list names))]
        | h -> h
      in
      Text (Printf.sprintf "type `%s` has no field `%s`" type_s field) :: codespan_local span :: hint
    | _ ->
      [
        Text (Printf.sprintf "type `%s` has no fields" type_s);
        codespan_local span;
        Note "only structs have fields"
      ]
  in
  raise_fatal (EventGraphError msg)

(* Reports a send or receive of [msg_spec] on the endpoint that cannot do it. *)
let check_direction (graph : event_graph) (ci : cunit_info) (msg_spec : message_specifier) (msg : message_def) (dir : message_direction) span =
  if msg.dir <> dir then (
    let open Except in
    let ep = MessageCollection.lookup_endpoint graph.messages msg_spec.endpoint |> Option.get in
    let (verb, opposite) = match dir with Out -> ("send", "receives") | Inp -> ("receive", "sends") in
    let declared_receiver =
      MessageCollection.lookup_channel_class ci.channel_classes ep.channel_class
      |> Fun.flip Option.bind (fun (cc : channel_class_def) ->
          List.find_opt (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages)
      |> Option.map (fun (m : message_def) -> if m.dir = Inp then Left else Right)
    in
    let declared_note = match declared_receiver with
      | Some r ->
        let other = if r = Left then Right else Left in
        [Note (Printf.sprintf "`%s` is declared `%s` in `%s`, so the %s endpoint sends it and the %s endpoint receives it"
          msg_spec.msg (side_name r) ep.channel_class (side_name other) (side_name r))]
      | None -> []
    in
    let other_help = match ep.opp with
      | Some o when MessageCollection.endpoint_owned graph.messages o ->
        [Help (Printf.sprintf "use `%s`, the other endpoint of this channel, to %s `%s`" o verb msg_spec.msg)]
      | _ -> []
    in
    raise (EventGraphError (
      [
        Text (Printf.sprintf "cannot %s `%s` on `%s`" verb msg_spec.msg msg_spec.endpoint);
        Label (None, span, Printf.sprintf "`%s` %s `%s`" msg_spec.endpoint opposite msg_spec.msg)
      ]
      @ declared_note
      @ [Note (Printf.sprintf "`%s` is the %s endpoint" msg_spec.endpoint (side_name ep.dir))]
      @ other_help))
  )

let binop_td_const graph (ci:cunit_info) _ctx span op n td =
  let w = unwrap_or_err "Invalid value" span td.ld.w in
  let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs td.ld.dtype in
  let sz' = match op with
  | Add -> sz+1
  | Mul -> sz + Utils.int_log2 n
  | _ -> sz in
  let (wires', w') = if sz' > sz then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
      ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w] wires'
  else (graph.wires, w) in
  let (wires'', wconst) = WireCollection.add_literal graph.thread_id
     ci.typedefs ci.macro_defs (WithLength (sz', n)) wires' in
  let (wires''', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w' (`Single wconst) wires'' in
  graph.wires <- wires''';
  {ld ={td.ld with w = Some wres; dtype = `Array (`Logic, ParamEnv.Concrete sz')}}


let binop_td_td graph (ci:cunit_info) ctx span op td1 td2 =
  let w1 = unwrap_or_err "Invalid value" span td1.ld.w in
  let w2 = unwrap_or_err "Invalid value" span td2.ld.w in
  let sz1 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td1.ld.dtype
  and sz2 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td2.ld.dtype in
  let sz' = match op with
  | Add -> (max sz1 sz2) +1
  | Mul -> sz1 + sz2
  | _ -> max sz1 sz2 in
  let (wires', w1') = if sz' > sz1 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz1, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w1] wires'
  else (graph.wires, w1) in
  let (wires', w2') = if sz' > sz2 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz2, 0)) wires' in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w2] wires'
  else (wires', w2) in
  let (wires', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w1' (`Single w2') wires' in
  graph.wires <- wires';
  let open Typing in
  let new_dtype = (`Array (`Logic, ParamEnv.Concrete sz')) in
  Typing.merged_data graph (Some wres) new_dtype ctx.current [td1.ld; td2.ld]


let rec lvalue_info_of graph (ci:cunit_info) ctx (e:expr_node) lval =
  let span = e.span in
  let binop_td_const = binop_td_const graph ci ctx span
  and binop_td_td = binop_td_td graph ci ctx span in
  match lval with
  | Reg ident ->
    let r = match Utils.StringMap.find_opt ident graph.regs with
      | Some r -> r
      | None -> not_a_register graph ci ctx ~reading:false ident span
    in
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d.d_type in
    AstAnnotator.attach_def_span_expr e r;
    {
      lval_range = full_reg_range ident sz;
      lval_dtype = r.d.d_type
    }
  | Indexed (lval', idx) ->
    (* TODO: better code reuse *)
    let lval_info' = lvalue_info_of graph ci ctx e lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (construct_graphIR graph ci ctx)
        (binop_td_const Mul)
        lval_info'.lval_dtype idx
      |> (function Some x -> x | None -> bad_index ci lval_info'.lval_dtype idx span) in
    let le'_nd = MaybeConst.map (fun ld -> {ld}) le' in
    let le_n = MaybeConst.map (fun nd -> nd.ld)
      (MaybeConst.add (binop_td_const Add) (binop_td_td Add) le'_nd le)
    in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs dtype in
      match type_def with
      | Some type_def -> AstAnnotator.attach_def_from_top_level_type e type_def
      | None -> ()
    );
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }
  | Indirected (lval', fieldname) ->
    let lval_info' = lvalue_info_of graph ci ctx e lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs lval_info'.lval_dtype fieldname.d
      |> (function Some x -> x | None -> bad_field ci lval_info'.lval_dtype fieldname.d span) in
    let le'_nd = MaybeConst.map (fun ld -> {ld}) le' in
    let le_n = MaybeConst.map (fun nd -> nd.ld)
      (MaybeConst.add_const le (binop_td_const Add) le'_nd)
    in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs dtype in (
        match type_def with
        | Some type_def -> AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(fieldname.d, fieldname)]
        | None -> ()
      );
    );
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }

and construct_graphIR (graph : event_graph) (ci : cunit_info)
                    (ctx : build_context) (e : expr_node) : node_data =
  let binop_td_const = binop_td_const graph ci ctx
  and _binop_td_td = binop_td_td graph ci ctx in
  match e.d with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) (dtype_of_literal lit :> data_type) ctx.current
  | Identifier ident ->
      let ctx_val = Typing.context_lookup ctx.cg_lt_ctx ident in
      let macro_val = List.assoc_opt ident (List.map (fun (macro : macro_def) ->(macro.id, macro)) ci.macro_defs) in
      (match ctx_val, macro_val with
        | Some a, Some b_def ->
          AstAnnotator.attach_def_span e a.binding_def_span;
          AstAnnotator.attach_def_from_top_level_macro e b_def;
          raise (event_graph_error_default ("Conflicting Identifier " ^ ident ^ " declarations found") e.span);
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        | Some binding, None ->
          AstAnnotator.attach_def_span e binding.binding_def_span;
          Typing.use_binding binding |> Typing.sync_data graph ctx.current
        | None, Some macro ->
          AstAnnotator.attach_def_from_top_level_macro e macro;
          let sz = Utils.int_log2 (macro.value + 1) in
          let (wires', w) = WireCollection.add_literal graph.thread_id
              ci.typedefs ci.macro_defs
              (WithLength (sz, macro.value)) graph.wires in
            graph.wires <- wires';
            Typing.const_data graph (Some w) (`Array (`Logic, ParamEnv.Concrete sz)) ctx.current
        | None, None -> unknown_value graph ci ctx ident e.span
      )

  | Assign (lval, e') ->
    let td = construct_graphIR graph ci ctx e' in
    let lvi = lvalue_info_of graph ci ctx e lval in
    let err_string = DTypeCheck.fmt_assign lval in
    check_dtype err_string (Some lvi.lval_dtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
    ctx.current.actions <- (RegAssign (lvi, td.ld) |> tag_with_span e.span)::ctx.current.actions;
    AstAnnotator.attach_event e ctx.current None (Some [DelayConst 1]);
    let data = Typing.cycles_data graph 1 ctx.current in
    set_origin data.ld.lt.live (Printf.sprintf "assignment to `%s`" (string_of_lvalue lval)) e.span;
    data
  | Call (id, arg_list) ->
      let func = match List.find_opt (fun (f: Lang.func_def) -> f.name = id) ci.func_defs with
        | Some f -> f
        | None ->
          let funcs = List.map (fun (f : Lang.func_def) -> (f.name, "function")) ci.func_defs in
          raise_fatal (EventGraphError (Except.Text (Printf.sprintf "no function named `%s`" id)
            :: Except.codespan_local e.span :: Except.did_you_mean id funcs))
      in
      AstAnnotator.attach_def_from_top_level_func e func;
      let td_args = List.map (construct_graphIR graph ci ctx) arg_list in
      let ctx' = BuildContext.clear_bindings ctx |> ref in
      if List.length td_args <> List.length func.args then
        let n_expected = List.length func.args and n_given = List.length td_args in
        let arg_s (a : typed_arg) = match a.arg_type with
          | Some t -> a.arg_name ^ " : " ^ string_of_data_type t
          | None -> a.arg_name
        in
        raise (EventGraphError [
          Text (Printf.sprintf "`%s` takes %d %s but %d %s given" id
            n_expected (if n_expected = 1 then "argument" else "arguments")
            n_given (if n_given = 1 then "was" else "were"));
          Except.codespan_local e.span;
          Except.Note (Printf.sprintf "`%s` is declared as `func %s(%s)`" id id
            (String.concat ", " (List.map arg_s func.args)))
        ])
      else List.iter2 (fun td arg ->
        (
          let _ = td.ld.w in (* added for tc*)
          match arg.arg_type with
            | Some gtype ->
              let err_string = DTypeCheck.fmt_func_arg id arg.arg_name in
              check_dtype err_string (Some gtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
            | None -> ()
        );
        ctx' := BuildContext.add_binding !ctx' arg.arg_name td.ld e.span
      ) td_args func.args;
      construct_graphIR graph ci !ctx' func.body
  | Binop (binop, e1, e2) ->
    (
      let td1 = construct_graphIR graph ci ctx e1 in
      let w1 = unwrap_or_err "Invalid value" e1.span td1.ld.w in
      match e2 with
      | `List ws2 ->
        let td2_list = List.map (fun e2 ->
          let td2 = construct_graphIR graph ci ctx e2 in
          td2
        ) ws2 in
        let w2_list = List.map (fun td2 ->
          unwrap_or_err "Invalid value of second operator" e1.span td2.ld.w
        ) td2_list in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`List w2_list) graph.wires in
        graph.wires <- wires';
        let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
        Typing.merged_data graph (Some w) new_dtype ctx.current (List.concat [[td1.ld]; List.map (fun td -> td.ld) td2_list])
      | `Single e ->
        let td2 = construct_graphIR graph ci ctx e in
        let w2 = unwrap_or_err "Invalid value of second operator" e1.span td2.ld.w in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`Single w2) graph.wires in
        graph.wires <- wires';
        let err_string = DTypeCheck.fmt_binop binop in
        let label = Printf.sprintf "the left side is `%s` but the right side is `%s`"
          (string_of_data_type td1.ld.dtype) (string_of_data_type td2.ld.dtype) in
        check_dtype ~label err_string (Some td1.ld.dtype) td2.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        
        let new_dtype = match binop with
        | LAnd | LOr | Lt | Gt | Lte | Gte | Eq | Neq ->
          `Logic
        | _ ->
          td1.ld.dtype
        in 
        
        Typing.merged_data graph (Some w) new_dtype ctx.current [td1.ld; td2.ld]
    )
  | Unop (unop, e') ->
    let td = construct_graphIR graph ci ctx e' in
    let w' = unwrap_or_err "Invalid value" e'.span td.ld.w in
    let (wires', w) = WireCollection.add_unary graph.thread_id ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td.ld
  | Tuple [] -> Typing.const_data graph None (unit_dtype) ctx.current
  | Let _ ->
    raise (Except.TypeError (GraphAnalysis.unused_let_message e.span));
    Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
  | Join (e1, e2) ->
    (
      match e1.d with
      | Let (["_"], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
      | Let ([], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
      | Let ([ident], dtype, inner_e) ->
          let td1 = construct_graphIR graph ci ctx inner_e in
          let err_string = DTypeCheck.fmt_let_binding ident in
          check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
          let ctx' = BuildContext.add_binding ctx ident td1.ld e1.span in
          let td = construct_graphIR graph ci ctx' e2 in
          (* check if the binding is used *)
          let binding = Typing.context_lookup ctx'.cg_lt_ctx ident |> Option.get in
          (* no need to wait if the value is current *)
          if td1.ld.lt.live.id <> ctx.current.id && not binding.binding_used then (
            Except.warn [
              Text (Printf.sprintf "the value bound to `%s` may never be waited for" ident);
              Label (None, e1.span, "this value takes time to compute");
              Note "after `;` the code continues without waiting for the value, so a path that does not use it can end before the value is ready";
              Help "use `>>` instead of `;` to wait for the value, or use it on every path"
            ]
          );
          td
      | Let _ ->
        raise (event_graph_error_default "Discarding expression results!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
      | _ ->
        let td1 = construct_graphIR graph ci ctx e1 in
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
    )
  | Wait (e1, e2) ->
    (
      match e1.d with
      | Let (["_"], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
      | Let ([], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
      | Let ([ident], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let_binding ident in
        check_dtype err_string dtype td1.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        (* add the binding to the context *)
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        let ctx' = BuildContext.add_binding ctx' ident td1.ld e1.span in
        construct_graphIR graph ci ctx' e2
      | Let _ ->
        raise (event_graph_error_default "Discarding expression results!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
      | _ ->
        let td1 = construct_graphIR graph ci ctx e1 in
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
    )
  | Ready msg_spec ->
    let msg = message_of graph ci msg_spec e.span in
    AstAnnotator.attach_def_from_top_level_message e msg msg_spec graph;
    AstAnnotator.attach_event e ctx.current None None;
    (* if msg.dir <> In then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    ); *)
    let wires, msg_valid_port = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_valid_port) `Logic ctx.current
  | Probe msg_spec ->
    let msg = message_of graph ci msg_spec e.span in
    AstAnnotator.attach_def_from_top_level_message e msg msg_spec graph;
    AstAnnotator.attach_event e ctx.current None None;
    let wires, msg_ack_port = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_ack_port) `Logic ctx.current
  | Cycle n_src ->
    let n = cycle_count ci ctx n_src e.span in
    let data = Typing.cycles_data graph n ctx.current in
    let n_s = match n_src with ParamEnv.Concrete v -> string_of_int v | ParamEnv.Param p -> p in
    set_origin data.ld.lt.live (Printf.sprintf "`cycle %s`" n_s) e.span;
    AstAnnotator.attach_event e ctx.current None (Some [DelayConst n]);
    data
  | IfExpr (e1, e2, e3) ->
    let td1 = construct_graphIR graph ci ctx e1 in
    (* TODO: type checking *)
    let w1 = unwrap_or_err "Invalid condition" e1.span td1.ld.w in
    let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
    let branch_info = {
      branch_cond_v = td1.ld;
      branch_cond = TrueFalse;
      branch_count = 2; (* for if-else *)
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx' branch_info 0 in
    let td2 = construct_graphIR graph ci ctx_true e2 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx' branch_info 1 in
    let td3 = construct_graphIR graph ci ctx_false e3 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td2.ld.lt.live; td3.ld.lt.live];

    BuildContext.branch_merge ctx' [ctx_true; ctx_false];
    AstAnnotator.attach_event e ctx.current None None;

    let ctx_br = BuildContext.branch graph ctx' branch_info in
    set_origin ctx_br.current "end of the `if`" e.span;
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td3.ld.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td3.ld.reg_borrows in
    (
      match td2.ld.w, td3.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w2, Some w3 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid if expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | TrySend (send_pack, e1, e2) ->
    (* data to send *)
    let td_send_data = construct_graphIR graph ci ctx send_pack.send_data in
    let wires, w_cond = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs send_pack.send_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;
    let msg = message_of graph ci send_pack.send_msg_spec e.span in
    check_direction graph ci send_pack.send_msg_spec msg Out e.span;
    let dtype = msg.sig_types |> List.hd |> (fun stype -> stype.dtype) in
    let err_string = DTypeCheck.fmt_send send_pack.send_msg_spec in
    check_dtype err_string (Some dtype) td_send_data.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx branch_info 0 in
    set_origin ctx_true.current
      (Printf.sprintf "`send %s.%s` in the `try`" send_pack.send_msg_spec.endpoint send_pack.send_msg_spec.msg) e.span;
    let td1 = construct_graphIR graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = construct_graphIR graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.ld.lt.live; td2.ld.lt.live];

    BuildContext.branch_merge ctx [ctx_true; ctx_false];

    ctx_true.current.actions <- (ImmediateSend (send_pack.send_msg_spec, td_send_data.ld) |> tag_with_span e.span)::ctx_true.current.actions;
    AstAnnotator.attach_event e ctx_true.current None None;

    (
      try let msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes
        |> unwrap_or_err "Invalid message specifier in try send" e.span
      in AstAnnotator.attach_def_from_top_level_message e msg send_pack.send_msg_spec graph;
      with _ -> ()
    );

    let ctx_br = BuildContext.branch graph ctx branch_info in
    set_origin ctx_br.current "end of the `try`" e.span;
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.ld.w, td2.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid try send expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | TryRecv (ident, recv_pack, e1, e2) ->
    let wires, w_cond = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs recv_pack.recv_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true_no_binding) = BuildContext.branch_side graph ctx branch_info 0 in
    set_origin ctx_true_no_binding.current
      (Printf.sprintf "`recv %s.%s` in the `try`" recv_pack.recv_msg_spec.endpoint recv_pack.recv_msg_spec.msg) e.span;

    (* message *)
    let msg = message_of graph ci recv_pack.recv_msg_spec e.span in
    let (wires', w_recv) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let stype = List.hd msg.sig_types in
    let d_recv = delay_pat_globalise recv_pack.recv_msg_spec.endpoint stype.lifetime.e in
    let td_recv = {
      w = w_recv;
      lt = {live = ctx_true_no_binding.current; dead = [(ctx_true_no_binding.current, d_recv)]};
      reg_borrows = [];
      dtype = stype.dtype;
    } in
    let ctx_true = BuildContext.add_binding ctx_true_no_binding ident td_recv e1.span in
    let td1 = construct_graphIR graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = construct_graphIR graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.ld.lt.live; td2.ld.lt.live];

    BuildContext.branch_merge ctx [ctx_true_no_binding; ctx_false];

    let action = ImmediateRecv recv_pack.recv_msg_spec |> tag_with_span e.span in
    ctx_true.current.actions <- action::ctx_true.current.actions;
    AstAnnotator.attach_event e ctx_true.current None None;
    AstAnnotator.attach_def_from_top_level_message e msg recv_pack.recv_msg_spec graph;

    let ctx_br = BuildContext.branch graph ctx branch_info in
    set_origin ctx_br.current "end of the `try`" e.span;
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.ld.w, td2.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid try recv expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )

  | Match (match_v, match_arms) ->
    let td_v = construct_graphIR graph ci ctx match_v in
    let w_v = unwrap_or_err "Invalid match expression" match_v.span td_v.ld.w in
    let ctx' = BuildContext.wait graph ctx td_v.ld.lt.live in
    let branch_info = {
      branch_cond_v = td_v.ld;
      branch_cond = MatchCases [];
      branch_count = List.length match_arms;
      branches_to = [];
      branches_val = [];
    } in

    let (default_arms, match_arms) =
      List.partition (fun ((pat, _) : expr_node * expr_node option) -> pat.d = Identifier "_") match_arms in
    (
      match default_arms with
      | [default_arm] ->
        let td_pats = List.map (fun (pat, _) -> construct_graphIR graph ci ctx' pat) match_arms in
        (* make sure that the patterns are all constants *)
        List.iter2
          (fun ((pat, _) : expr_node * expr_node option) td ->
            let valid =
              match td.ld.w with
              | None -> false
              | Some w -> w.is_const
            in
            if not valid then
              raise (event_graph_error_default "Match patterns must be constant values!" pat.span)
          ) match_arms td_pats;

        (* build bodies *)
        let branches = List.mapi
          (fun idx (_, body) ->
            let (br_side, ctx) = BuildContext.branch_side graph ctx' branch_info idx in
            let td = construct_graphIR graph ci ctx @@ Option.get body in
            ((ctx, br_side, td), (ctx.current, td.ld.lt.live))
          )
          match_arms
        in
        let (br_side_default, ctx_default) = BuildContext.branch_side graph ctx' branch_info (branch_info.branch_count - 1) in
        let td_default = construct_graphIR graph ci ctx_default @@ Option.get @@ snd default_arm in

        let (branches, branches_events) = List.split branches in
        let (branches_to, branches_val) = List.split branches_events in
        branch_info.branches_to <- branches_to @ [ctx_default.current];
        branch_info.branches_val <- branches_val @ [td_default.ld.lt.live];

        BuildContext.branch_merge ctx' @@ ctx_default::(List.map (fun (ctx, _, _) -> ctx) branches);
        let ctx_br = BuildContext.branch graph ctx' branch_info in
        set_origin ctx_br.current "end of the `match`" e.span;
        List.iter
          (fun (_, br_side, _) ->
            br_side.branch_event <- Some ctx_br.current
          )
          branches;
        br_side_default.branch_event <- Some ctx_br.current;
        let lt = {
          live = ctx_br.current;
          dead = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.ld.lt.dead) td_v.ld.lt.dead branches;
        } in
        let reg_borrows' = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.ld.reg_borrows) td_v.ld.reg_borrows branches in
        branch_info.branch_cond <- MatchCases (List.map (fun td -> td.ld) td_pats);
        if List.for_all (fun (_, _, td_val) -> Option.is_none td_val.ld.w) branches then
          {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
        else (
          let (wires', w) = WireCollection.add_cases graph.thread_id ci.typedefs w_v
            (List.map2 (fun td_pat (_, _, td_val) -> (Option.get td_pat.ld.w, Option.get td_val.ld.w)) td_pats branches) (match td_default.ld.w with | Some w -> w | None -> raise_fatal (event_graph_error_default "Invalid match expression (exactly one default case expected)!" e.span))
            graph.wires in
          graph.wires <- wires';
          {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td_default.ld.dtype}}
        )
      | _ ->
          raise @@ event_graph_error_default "Invalid match expression (atleast one default case expected)!" e.span;
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | Cast (e', dtype) ->
    let td = construct_graphIR graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in cast" e'.span td.ld.w in
    let target_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs dtype in
    if w.size > target_size then (
      (* Truncate: take slice of the wire to match target size *)
      let base_i = MaybeConst.Const 0 in
      let wires', w' = WireCollection.add_slice graph.thread_id w base_i target_size graph.wires in
      graph.wires <- wires';
      Typing.merged_data graph (Some w') dtype ctx.current [td.ld]
    ) else if w.size < target_size then (
      (* Extend: pad with zeros to match target size *)
      let pad_len = target_size - w.size in
      let wires', pad_w = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs (WithLength (pad_len, 0)) graph.wires in
      let wires', w' = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [pad_w; w] wires' in
      graph.wires <- wires';
      Typing.merged_data graph (Some w') dtype ctx.current [td.ld]
    ) else (
      (* Sizes match, just update type *)
      Typing.merged_data graph (Some w) dtype ctx.current [td.ld]
    )
  | Concat es ->
    let tds = List.map (fun e' -> (e', construct_graphIR graph ci ctx e')) es in
    let ws = List.map (fun ((e', td) : expr_node * node_data) -> unwrap_or_err "Invalid value in concat" e'.span td.ld.w) tds in
    let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
    graph.wires <- wires';
    let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
    AstAnnotator.attach_event e ctx.current None None;
    List.map (fun (_,td) -> td.ld) tds |> Typing.merged_data graph (Some w) new_dtype ctx.current
  |  Read rlval ->
    let reg_ident = Lang.get_lvalue_reg_id rlval in
    let r = match Utils.StringMap.find_opt reg_ident graph.regs with
      | Some r -> r
      | None -> not_a_register graph ci ctx ~reading:true reg_ident e.span
    in
    AstAnnotator.attach_def_span_expr e r;
    let (wires'', w'') = WireCollection.add_reg_read graph.thread_id ci.typedefs ci.macro_defs r.d graph.wires in
    graph.wires <- wires'';
    let td = {ld = {w = Some w''; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = r.d.d_type}} in
    let get_borrow_info in_off le dt w lval td' =
      match lval with 
        | Reg _ -> (dt,in_off,le,w,td')
        | Indexed (lv, idx) ->
          let inner_info = lvalue_info_of graph ci ctx e lv in
          (
            let type_def = TypedefMap.type_def_name_resolve ci.typedefs inner_info.lval_dtype in
            match type_def with
            | Some type_def ->
                AstAnnotator.attach_def_from_top_level_type e type_def
            | None -> ()
          );
          let inner_le = fst inner_info.lval_range.subreg_range_interval in
          let inner_le_nd = MaybeConst.map (fun ld -> {ld}) inner_le in
          let inner_dtype = inner_info.lval_dtype in
          let (offset_le, len, dt') = TypedefMap.data_type_index ci.typedefs ci.macro_defs
            (construct_graphIR graph ci ctx)
            (binop_td_const e.span Mul)
            inner_dtype idx |> (function Some x -> x | None -> bad_index ci inner_dtype idx e.span) in
          let total_offset_le = MaybeConst.add (binop_td_const e.span Add) (_binop_td_td e.span Add) offset_le inner_le_nd in
          let off_i = MaybeConst.map_off total_offset_le in
          let (off, le') = if off_i < 0 then (dynamic_index_warning reg_ident e.span; (0, len)) else (off_i, len) in
          let (wire', w') = WireCollection.add_slice graph.thread_id w (MaybeConst.map (fun td -> unwrap_or_err "Invalid indexing in lvalue" e.span td.ld.w) total_offset_le) le' graph.wires in
          graph.wires <- wire';
          let new_td = match total_offset_le with
            | MaybeConst.NonConst td_offset -> Typing.merged_data graph (Some w') dt' ctx.current [td'.ld; td_offset.ld]
            | MaybeConst.Const _ -> {ld = {td'.ld with w = Some w'; dtype = dt'}}
          in
          (dt', off, le', w', new_td)
        | Indirected (lval_inner, field_id) -> 
          let lval_info_inner = lvalue_info_of graph ci ctx e lval_inner in
          (
            let type_def = TypedefMap.type_def_name_resolve ci.typedefs lval_info_inner.lval_dtype in
            match type_def with
            | Some type_def ->
                AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(field_id.d, field_id)]
            | None -> ()
          );
          let (inner_le, _inner_len) = lval_info_inner.lval_range.subreg_range_interval in
          let inner_le_nd = MaybeConst.map (fun ld -> {ld}) inner_le in
          let (field_offset_le, len, new_dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs lval_info_inner.lval_dtype field_id.d
            |> (function Some x -> x | None -> bad_field ci lval_info_inner.lval_dtype field_id.d e.span) in
          (* total offset = inner offset + field offset *)
          let total_offset_le = MaybeConst.add_const field_offset_le (binop_td_const e.span Add) inner_le_nd in
          let off_i = MaybeConst.map_off total_offset_le in
          let (off, le') = if off_i < 0 then (dynamic_index_warning reg_ident e.span; (0, len)) else (off_i, len) in
          let (wi', new_w) = WireCollection.add_slice graph.thread_id w (MaybeConst.map (fun td -> unwrap_or_err "Invalid indexing in indirection" e.span td.ld.w) total_offset_le) le' graph.wires in
          graph.wires <- wi';
          let new_td = match total_offset_le with
            | MaybeConst.Const _ -> {ld = {td'.ld with w = Some new_w; dtype = new_dtype}}
            | MaybeConst.NonConst td_offset -> Typing.merged_data graph (Some new_w) new_dtype ctx.current [td'.ld; td_offset.ld]
          in
          (new_dtype, off, le', new_w, new_td)
    in
    let full_sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d.d_type in
    let (_dt,off,le,_w,td'') = get_borrow_info 0 full_sz r.d.d_type w'' rlval td in
    let borrow = {borrow_range = sub_reg_range reg_ident off le; borrow_start = ctx.current; borrow_source_span = e.span} in
    {ld = { td''.ld with dtype = _dt; reg_borrows = borrow :: td''.ld.reg_borrows }}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (construct_graphIR graph ci ctx) e_list in
        let all_w = List.for_all(
          fun td ->
            match td.ld.w with
            | Some _ -> true
            | None -> false
        ) timed_ws in 
        if not all_w then
          raise (Except.TypeError [Text "Invalid value in debug print"; Except.codespan_local e.span]);
        ctx.current.actions <- (let open EventGraph in DebugPrint (s, List.map (fun td -> td.ld) timed_ws) |> tag_with_span e.span)::ctx.current.actions;
        AstAnnotator.attach_event e ctx.current None None;
        {ld = {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}}
      | DebugFinish ->
        ctx.current.actions <- (let open EventGraph in tag_with_span e.span DebugFinish)::ctx.current.actions;
        AstAnnotator.attach_event e ctx.current None None;
        {ld = {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}}
    )
  | Send send_pack ->
    (* just check that the endpoint and the message type is defined *)
    let msg = message_of graph ci send_pack.send_msg_spec e.span in

    AstAnnotator.attach_def_from_top_level_message e msg send_pack.send_msg_spec graph;

    check_direction graph ci send_pack.send_msg_spec msg Out e.span;
    let td = construct_graphIR graph ci ctx send_pack.send_data in
    let msg_dtype = (List.hd msg.sig_types).dtype in
    let err_string = DTypeCheck.fmt_send send_pack.send_msg_spec in
    check_dtype err_string (Some msg_dtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    set_origin ntd.ld.lt.live
      (Printf.sprintf "`send %s.%s`" send_pack.send_msg_spec.endpoint send_pack.send_msg_spec.msg) e.span;
    ctx.current.sustained_actions <-
      ({
        until = ntd.ld.lt.live;
        ty = Send (send_pack.send_msg_spec, td.ld)
      } |> tag_with_span e.span)::ctx.current.sustained_actions;
    AstAnnotator.attach_event e ctx.current (Some ntd.ld.lt.live) None;
    ntd

  | Recv recv_pack ->
    let msg = message_of graph ci recv_pack.recv_msg_spec e.span in

    AstAnnotator.attach_def_from_top_level_message e msg recv_pack.recv_msg_spec graph;

    check_direction graph ci recv_pack.recv_msg_spec msg Inp e.span;
    let (wires', w) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph w recv_pack.recv_msg_spec msg ctx.current in
    set_origin ntd.ld.lt.live
      (Printf.sprintf "`recv %s.%s`" recv_pack.recv_msg_spec.endpoint recv_pack.recv_msg_spec.msg) e.span;
    ctx.current.sustained_actions <-
      ({until = ntd.ld.lt.live; ty = Recv recv_pack.recv_msg_spec} |> tag_with_span e.span)::ctx.current.sustained_actions;
    AstAnnotator.attach_event e ctx.current (Some ntd.ld.lt.live) None;
    ntd

  | Indirect (e', fieldname) ->
    let td = construct_graphIR graph ci ctx e' in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs td.ld.dtype in
      match type_def with
      | Some type_def ->
          AstAnnotator.attach_def_from_top_level_type_with_fields e' type_def [(fieldname.d, fieldname)]
      | None -> ()
    );
    let w = unwrap_or_err "Invalid value in indirection" e'.span td.ld.w in
    let (offset_le, len, new_dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs td.ld.dtype fieldname.d
      |> (function Some x -> x | None -> bad_field ci td.ld.dtype fieldname.d e.span) in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w (Const offset_le) len graph.wires in
    graph.wires <- wires';
    {ld = {
      td.ld with
      w = Some new_w;
      dtype = new_dtype
    }
    }
  | Index (e', ind) ->
    let td = construct_graphIR graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in indexing" e'.span td.ld.w in
    let (offset_le, len, new_dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (construct_graphIR graph ci ctx)
        (binop_td_const e.span Mul)
        td.ld.dtype ind
      |> (function Some x -> x | None -> bad_index ci td.ld.dtype ind e.span) in
    let wire_of (td:node_data) = unwrap_or_err (Printf.sprintf "Invalid indexing for %s in data type %s" (string_of_index ind) (Lang.string_of_data_type td.ld.dtype)) e.span td.ld.w in
    let offset_le_w = MaybeConst.map wire_of offset_le in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w offset_le_w len graph.wires in
    graph.wires <- wires';
    (
      match offset_le with
      | Const _ -> {ld = {td.ld with w = Some new_w; dtype = new_dtype}}
      | NonConst td_offset ->
          Typing.merged_data graph (Some new_w) new_dtype ctx.current [td.ld; td_offset.ld]
    )
  | Record (record_ty_name, field_exprs, None) ->
    (
      match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some type_def ->
          let field_exprs_flattened = List.map (fun ({d = (ident, _); _} as n) -> (ident, n)) field_exprs in
          AstAnnotator.attach_def_from_top_level_type_with_fields e type_def field_exprs_flattened
      | None -> ()
    );

    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder
            (List.map (fun n -> fst n.d) record_fields)
            (List.map (fun n -> n.d) field_exprs) with
          | Some expr_reordered ->
            let tds = List.map2 (fun {d = (field_name, expected_dtype); _} e' ->
              let td = construct_graphIR graph ci ctx e' in
              let err_string = DTypeCheck.fmt_record_field field_name in
              check_dtype err_string (Some expected_dtype) td.ld.dtype e'.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
              (e', td)
            ) record_fields expr_reordered in
            let ws = List.rev_map (fun ((e', td) : expr_node * node_data) ->
              unwrap_or_err "Invalid value in record field" e'.span td.ld.w) tds in
            let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
            graph.wires <- wires';
            List.map (fun (_, td) -> td.ld) tds |> Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current
          | _ ->
            raise (event_graph_error_default "Invalid record type value!" e.span);
            Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        )
      | _ ->
          raise (event_graph_error_default "Invalid record type name!" e.span);
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | Record (record_ty_name, field_exprs, Some field_base) ->
      (
        match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
        | Some type_def ->
            let field_exprs_flattened = List.map (fun ({d = (ident, _); _} as n) -> (ident, n)) field_exprs in
            AstAnnotator.attach_def_from_top_level_type_with_fields e type_def field_exprs_flattened
        | None -> ()
      );

      (* record update *)
      let td_base = construct_graphIR graph ci ctx field_base in
      let tds = List.map (fun {d = (field_ident, e'); _} -> (field_ident, e', construct_graphIR graph ci ctx e')) field_exprs in
      let updates =
        (* bruteforce *)
        List.map (fun (field_ident, _e', td) ->
          match TypedefMap.data_type_indirect ci.typedefs ci.macro_defs (`Named (record_ty_name, [])) field_ident with
          | None -> 
            let err_string = Printf.sprintf "In record update: Invalid field %s for record type %s" field_ident record_ty_name in
             raise_fatal (event_graph_error_default err_string e.span)
          | Some (offset_le, len, _dtype) -> (offset_le, len, Option.get td.ld.w)
        ) tds in
      let (wires', w) = WireCollection.add_update graph.thread_id ci.typedefs (Option.get td_base.ld.w) updates graph.wires in
      graph.wires <- wires';
      Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current (td_base.ld::(List.map (fun (_, _, td) -> td.ld) tds))
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some type_def -> AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(cstr_spec.variant.d, cstr_spec.variant)]
      | None -> ()
    );
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some (`Variant (dtype_opt, variants)) ->
        let variant_val = `Variant (dtype_opt, variants) in
        let e_dtype_opt = variant_lookup_dtype variant_val cstr_spec.variant.d in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = construct_graphIR graph ci ctx cstr_expr in
            (* if td.dtype <> e_dtype then
              raise (Except.TypeError [Text ("In variant construction: Invalid data type for " ^ cstr_spec.variant ^ ": expected " ^ (string_of_data_type e_dtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span]); *)
            let w = unwrap_or_err "Invalid value in variant construction" cstr_expr.span td.ld.w in
            let tag_size = variant_tag_size variant_val
            and data_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs e_dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs variant_val
            and var_idx = variant_lookup_index variant_val cstr_spec.variant.d
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant.d) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size + data_size then
              (* no padding *)
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w; w_tag] wires'
            else begin
              (* padding needed *)
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size - data_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w; w_tag] wires'
            end in
            graph.wires <- wires';
            {ld = { td.ld with w = Some new_w }}
          | None, None ->
            let tag_size = variant_tag_size variant_val
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs variant_val
            and var_idx = variant_lookup_index variant_val cstr_spec.variant.d
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant.d) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size then
              (wires', w_tag)
            else begin
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w_tag] wires'
            end in
            graph.wires <- wires';
            Typing.const_data graph (Some new_w)  (`Named (cstr_spec.variant_ty_name, [])) ctx.current
          | _ ->
            raise (event_graph_error_default "Invalid variant construct expression!" e.span);
            Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        )
      | _ ->
        raise (event_graph_error_default "Invalid variant type name!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | List li ->
    let tds = List.map (construct_graphIR graph ci ctx) li in
    let ws = List.map (fun td -> unwrap_or_err "Invalid wires!" e.span td.ld.w) tds in
    let (wires', new_w) = WireCollection.add_list graph.thread_id ci.typedefs ws graph.wires
      |> unwrap_or_err "Invalid list!" e.span in
    graph.wires <- wires';
    let td = List.hd tds in
    Typing.merged_data graph (Some new_w) (`Array (td.ld.dtype, ParamEnv.Concrete (List.length tds))) ctx.current (List.map (fun td -> td.ld) tds)
  | Recurse ->
    ctx.current.is_recurse <- true;
    Typing.const_data graph None unit_dtype ctx.current
  | Tuple _ ->
    raise (event_graph_error_default "Unimplemented expression!" e.span);
    Typing.const_data graph None unit_dtype ctx.current (* dummy return *)
