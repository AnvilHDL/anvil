open Lang
open EventGraph
open GraphAnalysis
open ErrorCollector

let quote_msg (m : message_specifier) = Printf.sprintf "`%s.%s`" m.endpoint m.msg

(* "`send ep.req`" or "`recv ep.req`" *)
let describe_msg_use (sa : sustained_action) =
  match sa.ty with
  | Send (m, _) -> Printf.sprintf "`send %s.%s`" m.endpoint m.msg
  | Recv m -> Printf.sprintf "`recv %s.%s`" m.endpoint m.msg

(** Check if the uses of endpoints and registers follow defined order. *)
let check_linear (config : Config.compile_config) lookup_message (g : event_graph) =
  let events_rev = List.rev g.events in
  let reg_ops = ref [] in (* all uses of registers *)
  let msg_uses = Hashtbl.create 2 in (* uses of messages *)
  let string_of_msg msg = Printf.sprintf "%s.%s" msg.endpoint msg.msg in (* serialise messagse *)
  let add_msg msg ev sa_span =
    let msg_str = string_of_msg msg in
    match Hashtbl.find_opt msg_uses msg_str with
    | Some li -> li := (ev, sa_span)::!li
    | None -> Hashtbl.add msg_uses msg_str (ref [(ev, sa_span)])
  in
  (* gather register and message uses *)
  List.iter
    (fun (ev : event) ->
      List.iter (fun ac_span ->
        match ac_span.d with
        | DebugFinish -> ()
        | DebugPrint (_, _ds) -> ()
          (* List.iter (add_reg_ops_td ev) ds *)
        | RegAssign (lv, _td) ->
          (* add_reg_ops_td ev td; *)
          reg_ops := (ev, lv.lval_range, ac_span.span)::!reg_ops
        | ImmediateRecv msg ->
          add_msg msg ev ({d = {ty = Recv msg; until = ev}; def_span = ac_span.def_span; action_event = ac_span.action_event; span = ac_span.span})
        | ImmediateSend (msg, td) ->
          add_msg msg ev ({d = {ty = Send (msg, td); until = ev}; def_span = ac_span.def_span; action_event = ac_span.action_event; span = ac_span.span})
          (* add_reg_ops_td ev td *)
      ) ev.actions;
      List.iter (fun sa_span ->
        match sa_span.d.ty with
        | Send (msg, _)
        | Recv msg -> add_msg msg ev sa_span
      ) ev.sustained_actions
    )
    events_rev;

  if config.verbose then (
    List.iter (fun (ev, range, _span) ->
      let range_s = match range.subreg_range_interval with
      | (Const n, sz) -> Printf.sprintf "%s[%d, %d]" range.subreg_name n sz
      | (NonConst _, sz) -> Printf.sprintf "%s[var, %d]" range.subreg_name sz
      in
      Printf.eprintf "RegAssign at %d to %s\n" ev.id range_s
    ) !reg_ops;
    Hashtbl.iter (fun msg_str li ->
      Printf.eprintf "Uses of %s:\n" msg_str;
      List.iter (fun (ev, sa_span) ->
        Printf.eprintf "  %d -> %d\n" ev.id sa_span.d.until.id
      ) !li
    ) msg_uses
  );

  (* check for violations of linearity for register assignments *)
  let rec check_reg_violation = function
    | (ev, range, span)::remaining ->
      List.iter (fun (ev', range', span') ->
        if (EventGraphOps.subreg_ranges_possibly_intersect range range') &&
           (events_get_order g.events lookup_message ev ev' |> is_strict_ordered |> not) then
            let reg = range.subreg_name in
            let open Except in
            raise (LifetimeCheckError (
              if span = span' then [
                Text (Printf.sprintf "an assignment to `%s` can overlap with itself in the next loop iteration" reg);
                Label (None, span, "this assignment may still be in progress when the next iteration starts it again");
                Note "assignments to the same register must happen one after another";
                Help "make the loop iteration wait for the assignment with `>>`"
              ] else [
                Text (Printf.sprintf "`%s` may be assigned twice at the same time" reg);
                Label (None, span, Printf.sprintf "this assignment to `%s`" reg);
                Label (None, span', "may happen at the same time as this one");
                Note "assignments to the same register must happen one after another";
                Help "order them with `>>`, or put them in different branches of an `if`"
              ]))
      ) remaining;
      check_reg_violation remaining
    | [] -> ()
  in
  check_reg_violation !reg_ops;

  (* check for violations of linearity for message uses *)
  Hashtbl.iter (fun msg_str li ->
    let rec check_msg_violation = function
      | (ev, sa_span)::li' ->
        List.iter (fun (ev', sa_span') ->
          let order1 = events_get_order g.events lookup_message ev sa_span'.d.until in
          let order2 = events_get_order g.events lookup_message sa_span.d.until ev' in
          let order_to_sign = function
            | Before | BeforeEq -> -1
            | After | AfterEq -> 1
            | AlwaysEq | Unreachable -> 0
            | Unordered -> -2
          in
          let check_ok =
            match order_to_sign order1, order_to_sign order2 with
            | -2, _ | _, -2 -> false
            | 1, _ | _, -1 -> true
            | 0, _ | _, 0 -> true
            | _ -> false
          in
          if not check_ok then (
            let open Except in
            raise (LifetimeCheckError (
              if sa_span.span = sa_span'.span then [
                Text (Printf.sprintf "%s can overlap with itself in the next loop iteration" (describe_msg_use sa_span.d));
                Label (None, sa_span.span, "this may not have finished when the next iteration starts it again");
                Note "uses of the same message must happen one after another";
                Help "wait for it with `>>`, so that the loop iteration ends only after it completes"
              ] else [
                Text (Printf.sprintf "`%s` may be used twice at the same time" msg_str);
                Label (None, sa_span.span, "this " ^ describe_msg_use sa_span.d);
                Label (None, sa_span'.span, "may overlap with this " ^ describe_msg_use sa_span'.d);
                Note "uses of the same message must happen one after another";
                Help "order them with `>>`"
              ]))
          )
        ) li';
        check_msg_violation li'
      | [] -> ()
    in
    check_msg_violation !li
  ) msg_uses

module IntHashtbl = Hashtbl.Make(Int)

(* Newer and more rigorous algorithm. *)
let event_pat_rel2 events lookup_message ev_pat1 ev_pat2 =
  let get_points_dist (ev, d_pat) =
    match d_pat with
    | `Cycles n -> ([ev], n)
    | `Eternal -> ([ev], -1)
    | `Message_with_offset (m, offset, sign) ->
      let g = GraphAnalysis.events_first_msg events ev m in
      match sign with
      | true -> (g, offset)

      | false -> match ((lookup_message m): message_def option) with
        | Some def -> raise_fatal (LifetimeCheckError [
          Text "a message lifetime with a negative offset is not supported yet";
          Except.Label (code_span_file_name def.span, def.span, "this message is declared with a negative offset")
        ])
        | None -> raise_fatal (Except.internal_error (Printf.sprintf "Message_def_not_found %s" (string_of_msg_spec m)))
  in
  match ev_pat1 with
  | [spat1] -> (
    (* source_points is the set of source points to consider *)
    (* dist1 is the distance required, -1 to indicate eternal *)
    let (source_points, dist1) = get_points_dist spat1 in
    (* Printf.eprintf "Sources %d (%d) = " (fst spat1).id dist1;
    List.iter (fun source -> Printf.eprintf "%d " source.id) source_points;
    Printf.eprintf "\n"; *)
    let targets_list = List.map get_points_dist ev_pat2 in
    if dist1 = -1 then
      (* If eternal *)
      true
      (* List.for_all (fun (_, dist2) -> dist2 = -1) targets_list *)
    else (
      (* dist1 != -1 *)
      List.for_all (fun (target_points, dist2) ->
        if dist2 = -1 then
          true
        else (
          (* Printf.eprintf "Targets (%d) = " dist2;
          List.iter (fun target -> Printf.eprintf "%d " target.id) target_points;
          Printf.eprintf "\n"; *)
          (* neither dist2 nor dist1 is -1 *)
          List.for_all (fun target ->
            let slacks = GraphAnalysis.events_max_dist events lookup_message target in
            List.for_all (fun source -> slacks.(source.id) <= dist2 - dist1) source_points
          ) target_points
        )
      ) targets_list
      (* List.for_all (fun source ->
        let slacks = GraphAnalysis.events_max_dist events source in
        List.for_all (fun (target_points, dist2) ->
          if dist2 = -1 then
            true
          else (
            (* neither dist2 nor dist1 is -1 *)
            List.for_all (fun target -> slacks.(target.id) <= dist2 - dist1) target_points
          )
        ) targets_list
      ) source_points *)
    )
  )
  | _ -> false (* not to be supported *)

(** Check that lt1 is always fully covered by lt2 *)
let lifetime_in_range events lookup_message (lt1 : lifetime) (lt2 : lifetime) =
  (* 1. check if lt2's start is a predecessor of lt1's start *)
  (* 2. derive a set of all time points A potentially within lt1 *)
  (* 4. check that end time of lt2 does not match any time point in A *)
  (* (event_is_successor lt2.live lt1.live) && (
    let r = event_pat_matches lt1.live lt2.dead in
    (not r.at) && (not r.aft)
  ) *)
  (event_pat_rel2 events lookup_message [(lt2.live, `Cycles 0)] [(lt1.live, `Cycles 0)])
    && (event_pat_rel2 events lookup_message lt1.dead lt2.dead)

(** Definitely disjoint? *)
let lifetime_disjoint events lookup_message lt1 lt2 =
  assert (List.length lt1.dead = 1);
  (* if the lifetimes start at mutually unreachable events, the lifetimes are disjoint *)
  let reachable = events_reachable events lt1.live in
  (* to be disjoint, either r1 <= l2 or r2 <= l1 *)
  not reachable.(lt2.live.id)
    || (event_pat_rel2 events lookup_message lt1.dead [(lt2.live, `Cycles 0)])
    || (List.for_all (fun de -> event_pat_rel2 events lookup_message [de] [(lt1.live, `Cycles 0)]) lt2.dead)


(* An internal identifier for a message specifier. *)
let msg_ident msg = Printf.sprintf "%s@%s" msg.endpoint msg.msg

let msg_spec_of_ident ident =
  match String.split_on_char '@' ident with
  | endpoint :: rest -> {endpoint; msg = String.concat "@" rest}
  | [] -> {endpoint = ident; msg = ""}

(* A message lifetime as written in a channel class, e.g. "@#1" or "@req". *)
let string_of_chan_lifetime (pat : delay_pat_chan_local) =
  match pat with
  | `Cycles n -> Printf.sprintf "@#%d" n
  | `Eternal -> "eternal"
  | `Message_with_offset (m, 0, _) -> "@" ^ m
  | `Message_with_offset (m, o, plus) -> Printf.sprintf "@#%s%s%d" m (if plus then "+" else "-") o

let span_contains (outer : code_span) (inner : code_span) =
  outer.st.pos_cnum <= inner.st.pos_cnum && inner.ed.pos_cnum <= outer.ed.pos_cnum

(* Events joined into [ev] that have not been reached by [at]. *)
let pending_events events lookup_message (ev : event) (at : event) =
  let rec leaves (e : event) =
    match e.origin, e.source with
    | None, `Later (a, b) -> leaves a @ leaves b
    | _ -> [e]
  in
  leaves ev
  |> List.sort_uniq (fun (a : event) (b : event) -> compare a.id b.id)
  |> List.filter (fun e -> not (event_pat_rel2 events lookup_message [(e, `Cycles 0)] [(at, `Cycles 0)]))

(* Error for a value whose lifetime does not cover the lifetime [needed] by its use. *)
let lifetime_error events lookup_message ~what ~use_span ~use_label ~(needed : lifetime) ~(value : lifetime) ~help =
  let open Except in
  if not (event_pat_rel2 events lookup_message [(value.live, `Cycles 0)] [(needed.live, `Cycles 0)]) then (
    let pending = pending_events events lookup_message value.live needed.live in
    let labels = List.filter_map (fun (e : event) ->
        Option.map (fun (o : event_origin) ->
          Label (None, o.where, Printf.sprintf "the value is only available after this %s" o.what)) e.origin
      ) pending in
    [
      Text (Printf.sprintf "%s is used before it is available" what);
      Label (None, use_span, "the value is used here")
    ]
    @ (if labels = [] then
         [Note (Printf.sprintf "the value is only available from %s" (EventGraphOps.describe_event value.live))]
       else labels)
    @ [Help "wait for the value with `>>` before using it, e.g. `let x = ... >> dprint \"%d\" (x)`"]
  ) else (
    let limiting = List.filter (fun d -> not (event_pat_rel2 events lookup_message needed.dead [d])) value.dead in
    let sources = List.map (fun (ev, pat) ->
        match ev.origin with
        | Some o ->
          Label (None, o.where, Printf.sprintf "the value depends on this %s and is only valid until %s"
            o.what (EventGraphOps.describe_pat_after pat "it"))
        | None ->
          Note (Printf.sprintf "the value is only valid until %s"
            (EventGraphOps.describe_pat_after pat (EventGraphOps.describe_event ev)))
      ) limiting in
    [
      Text (Printf.sprintf "%s does not live long enough" what);
      Label (None, use_span, use_label)
    ]
    @ sources
    @ help limiting
  )

module StringHashtbl = Hashtbl.Make(String)

let lifetime_check (config : Config.compile_config) (ci : cunit_info) (g : event_graph) =
  let lookup_message msg = MessageCollection.lookup_message g.messages msg ci.channel_classes in

  if config.verbose then (
    Printf.eprintf "==== Lifetime Check Details ====\n";
    EventGraphOps.print_graph g;
    EventGraphOps.print_dot_graph g Out_channel.stderr
  );

  GraphAnalysis.events_prepare_outs g.events;

  (* check that it takes at least one cycle to execute *)
  let root_ev = List.find (fun e -> e.source = `Root None) g.events in
  let last_ev = List.hd g.events in (* assuming reverse topo order *)
  let dist = event_min_distance g.events root_ev root_ev in
  if IntHashtbl.find dist last_ev.id = 0 then
    raise (LifetimeCheckError (GraphAnalysis.zero_cycle_loop_message g.thread_codespan));

  check_linear config lookup_message g;

  let reg_borrows = StringHashtbl.create 8 in (* regname -> (lifetime, range, use span)*)
  let msg_borrows = StringHashtbl.create 8 in
  (* check lifetime for each use of a wire *)
  let intersects_borrowed_reg s lt range =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    List.filter (fun (lt_tagged', range', _) ->
      (EventGraphOps.subreg_ranges_possibly_intersect range range') && (lifetime_disjoint g.events lookup_message lt lt_tagged'.d |> not))
      borrows
  in
  let intersects_borrowed_msg_and_add s lt_tagged =
    let lt = lt_tagged.d in
    let borrows = StringHashtbl.find_opt msg_borrows s |> Option.value ~default:[] in
    let intersects = List.filter (fun lt_tagged' -> lifetime_disjoint g.events lookup_message lt lt_tagged'.d |> not) borrows in
    if intersects = [] then StringHashtbl.replace msg_borrows s (lt_tagged::borrows);
    intersects
  in
  let borrow_add_reg s lt_tagged range use_span =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    StringHashtbl.replace reg_borrows s ((lt_tagged, range, use_span)::borrows)
  in
  let visit_actions a_visitor sa_visitor =
    List.iter (fun ev ->
      List.iter (a_visitor ev) ev.actions;
      List.iter (sa_visitor ev) ev.sustained_actions
    )
  in
  (* add all required borrows of registers first *)
  (* the time pat until which td lives *)
  let delay_pat_reduce_cycles n dpat =
    match dpat with
    | `Cycles n' -> `Cycles (max 0 (n' - n))
    | `Message_with_offset (m, o, true) -> `Message_with_offset (m, max 0 (o - n), true)
    | _ -> dpat
  in
  let td_to_live_until = ref [] in
  visit_actions
    (fun ev a ->
      match a.d with
      | RegAssign (lval_info, td) ->
        (* `Cycles 0 rather than 1 because in the last cycle it is okay to update the register *)
        td_to_live_until := (td, (ev, `Cycles 0), a.span)::!td_to_live_until;
        let (range_st, _) = lval_info.lval_range.subreg_range_interval in
        (
          match range_st with
          | Const _ -> ()
          | NonConst range_st_td ->
            td_to_live_until := (range_st_td, (ev, `Cycles 0), a.span)::!td_to_live_until
        )
      | DebugPrint (_, tds) ->
        let ns = List.map (fun td -> (td, (ev, `Cycles 0), a.span)) tds in
        td_to_live_until := Utils.list_unordered_join ns !td_to_live_until
      | DebugFinish -> ()
      | ImmediateSend (msg, td) ->
        let msg_d = lookup_message msg |> Option.get in
        let stype = List.hd msg_d.sig_types in
        let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e |> delay_pat_reduce_cycles 1 in
        td_to_live_until := (td, (ev, e_dpat), a.span)::!td_to_live_until
      | ImmediateRecv _ -> ()
    )
    (fun _ev sa ->
      match sa.d.ty with
      | Send (msg, td) ->
        let msg_d = lookup_message msg |> Option.get in
        let stype = List.hd msg_d.sig_types in
        let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e |> delay_pat_reduce_cycles 1 in
        td_to_live_until := (td, (sa.d.until, e_dpat), sa.span)::!td_to_live_until
      | Recv _ -> ()
    )
    g.events;
  List.iter (fun (td, dead, use_span) ->
    List.iter (fun borrow ->
      let lt_tagged = tag_with_span borrow.borrow_source_span {live = borrow.borrow_start; dead = [dead]} in
      borrow_add_reg borrow.borrow_range.subreg_name lt_tagged borrow.borrow_range use_span
    ) td.reg_borrows
  ) !td_to_live_until;
  let open Except in
  let channel_class_of (m : message_specifier) =
    MessageCollection.lookup_endpoint g.messages m.endpoint
    |> Option.map (fun (ep : endpoint_def) -> ep.channel_class) |> Option.value ~default:""
  in
  let recv_lifetime_help limiting =
    List.filter_map (fun ((ev : event), _) ->
      match ev.source with
      | `Seq (_, `Recv m) ->
        lookup_message m |> Option.map (fun (md : message_def) ->
          Help (Printf.sprintf "alternatively, declare a longer lifetime for `%s` in channel class `%s`; it is `%s` now"
            m.msg (channel_class_of m) (string_of_chan_lifetime (List.hd md.sig_types).lifetime.e)))
      | _ -> None
    ) limiting
    |> List.sort_uniq compare
  in
  let value_error ~what ~use_span ~use_label ~needed ~value ~fix =
    raise (LifetimeCheckError (lifetime_error g.events lookup_message ~what ~use_span ~use_label ~needed ~value
      ~help:(fun limiting -> Help fix :: recv_lifetime_help limiting)))
  in
  (* check register and message borrows *)
  let check_send msg td ev ev_until span =
    let msg_d = lookup_message msg |> Option.get in
    let stype = List.hd msg_d.sig_types in
    let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e in
    let lt = {live = ev; dead = [(ev_until, e_dpat)]} in
    let msg_s = quote_msg msg in
    (
      match intersects_borrowed_msg_and_add (string_of_msg_spec msg) (tag_with_span span lt) with
      | lt_intersect_tagged::_ ->
        let previous =
          if lt_intersect_tagged.span = span then
            [Label (None, span, "this `send` can happen again in the next loop iteration before the value it sent expires")]
          else [
            Label (None, span, "this `send` ...");
            Label (None, lt_intersect_tagged.span, "... can happen before the value sent here expires")
          ]
        in
        raise (LifetimeCheckError (
          Text (Printf.sprintf "%s may be sent again while the previous value is still valid" msg_s)
          :: previous
          @ [
            Note (Printf.sprintf "a value sent on %s stays valid until %s (`%s` in channel class `%s`)"
              msg_s (EventGraphOps.describe_pat_after e_dpat "it is sent") (string_of_chan_lifetime stype.lifetime.e) (channel_class_of msg));
            Help "send again only after the previous value has expired"
          ]))
      | [] -> ()
    );
    if lifetime_in_range g.events lookup_message lt td.lt |> not then
      value_error ~what:("value sent on " ^ msg_s) ~use_span:span
        ~use_label:(Printf.sprintf "the value is sent here and must stay valid until %s" (EventGraphOps.describe_pat_after e_dpat "it is sent"))
        ~needed:lt ~value:td.lt
        ~fix:"store the value in a register while it is still valid (`set r := ...`) and send `*r` instead"
  in
  visit_actions
    (fun ev a ->
      match a.d with
      | RegAssign (lval_info, td) ->
        let lt = EventGraphOps.lifetime_immediate ev in
        let reg = lval_info.lval_range.subreg_name in
        (
          match intersects_borrowed_reg reg lt lval_info.lval_range with
          | [] -> ()
          | (lt_tagged, _, use_span)::_ ->
            let until = match List.hd lt_tagged.d.dead with
              | (_, `Cycles 0) -> ""
              | (ev, pat) -> Printf.sprintf ", and must stay unchanged until %s"
                (EventGraphOps.describe_pat_after pat (EventGraphOps.describe_event ev))
            in
            let use_labels =
              if span_contains use_span lt_tagged.span then
                [Label (None, lt_tagged.span, Printf.sprintf "`%s` is read here%s" reg until)]
              else [
                Label (None, lt_tagged.span, Printf.sprintf "`%s` is read here" reg);
                Label (None, use_span, Printf.sprintf "the value read is still used here%s" until)
              ]
            in
            raise (LifetimeCheckError (
              [
                Text (Printf.sprintf "cannot assign to `%s` while a value read from it is still in use" reg);
                Label (None, a.span, Printf.sprintf "`%s` is assigned here" reg)
              ]
              @ use_labels
              @ [
                Note (Printf.sprintf "reading `%s` with `*` does not copy it, so `%s` must not change until the value read is no longer needed" reg reg);
                Help (Printf.sprintf "assign `%s` after the last use of the value, or copy the value into another register first" reg)
              ]))
        );
        if lifetime_in_range g.events lookup_message lt td.lt |> not then
          value_error ~what:(Printf.sprintf "value assigned to `%s`" reg) ~use_span:a.span
            ~use_label:"the value is assigned here" ~needed:lt ~value:td.lt
            ~fix:"assign the value earlier, while it is still valid";
        (
          match fst lval_info.lval_range.subreg_range_interval with
          | Const _ -> ()
          | NonConst range_st_td ->
            if lifetime_in_range g.events lookup_message lt range_st_td.lt |> not then
              value_error ~what:(Printf.sprintf "index into `%s`" reg) ~use_span:a.span
                ~use_label:"the index is used here" ~needed:lt ~value:range_st_td.lt
                ~fix:"use the index while it is still valid, or store it in a register first"
        )
      | DebugPrint (_, tds) ->
        let n = List.length tds in
        List.iteri (fun i td ->
          let lt = EventGraphOps.lifetime_immediate ev in
          if lifetime_in_range g.events lookup_message lt td.lt |> not then
            value_error
              ~what:(if n > 1 then Printf.sprintf "printed value %d of %d" (i + 1) n else "printed value")
              ~use_span:a.span ~use_label:"the value is printed here" ~needed:lt ~value:td.lt
              ~fix:"print the value earlier, while it is still valid"
        ) tds
      | DebugFinish -> ()
      | ImmediateSend (msg, td) ->
        check_send msg td ev ev a.span
      | ImmediateRecv _ -> ()
    )
    (fun ev sa ->
      match sa.d.ty with
      | Send (msg, td) ->
        check_send msg td ev sa.d.until sa.span
      | Recv _ -> ()
    )
    g.events;

  (* static sync pattern checks; we check the path between adjacent message send/recv *)
  (* collect all messages that require checking first *)
  (* First: find out all messages that require checks. Store inside msg_to_check,
    which maps an internal message identifier to the number of cycles in delay *)
  let msg_to_check = ref Utils.StringMap.empty in
  let get_msg_gap is_send msg =
    let msg_def = lookup_message msg |> Option.get in
    let (sync_mode, other_sync_mode) = if is_send then
      (msg_def.send_sync, msg_def.recv_sync)
    else
      (msg_def.recv_sync, msg_def.send_sync) in
    (
      match sync_mode, other_sync_mode with
      | Static (o, n), Static _ -> Some (o, msg, n, true, true)
      | Static (o, n), Dynamic -> Some (o, msg, n, true, false)
      | Dynamic, Static (o, n) -> Some (o, msg, n, false, true)
      | Dependent (m, n), Dependent _ -> Some (0, {msg with msg = m}, n, true, true)
      | Dependent (m, n), Dynamic -> Some (0, {msg with msg = m}, n, true, false)
      | Dynamic, Dependent (m, n) -> Some (0, {msg with msg = m}, n, false, true)
      | _ -> None
    ) |> Option.map (
      function
      | (o, m, n, l, r) -> (o, msg_ident m, n, l, r)
    )

  in
  visit_actions
          (fun _ _ -> ())
          (fun _ev sa ->
            let (msg, msg_gap) = match sa.d.ty with
            | Send (msg, _) -> (msg, get_msg_gap true msg)
            | Recv msg -> (msg, get_msg_gap false msg)
            in
            match msg_gap with
            | None -> ()
            | Some n -> (
              msg_to_check := Utils.StringMap.add (msg_ident msg) n !msg_to_check
            )
          )
          g.events;
  (* Now we have all messages we need to check. *)
  if config.verbose then (
    Config.debug_println config "Messages requiring sync mode checks below:";
    Utils.StringMap.iter
      (fun m (o, relative_msg, n, self_check, other_check) ->
        Printf.sprintf "Message %s with init offset %d, gap %d, relative to %s(<=: %b, >=: %b)"
            m o n relative_msg self_check other_check
        |> Config.debug_println config)
    !msg_to_check
  );
  (* Check per message *)
  let check_msg_sync_mode msg (init_offset, relative_msg, gap, self_check, other_check) =
    (* To Do: Generalize Check if this is a one-cycle message with Static #1 #1 sync mode (combinational) *)
    let msg_spec = msg_spec_of_ident msg in
    let msg_def_opt = lookup_message msg_spec in
    let is_one_cycle_static_1_1 = match msg_def_opt with
      | Some msg_def ->
        (match msg_def.send_sync, msg_def.recv_sync with
        | Static (0, 1), Static (0, 1) ->
          (* Check if message lifetime is one cycle *)
          (match List.hd msg_def.sig_types with
          | {lifetime = {e = `Cycles 1}; _} -> true
          | {lifetime = {e = `Eternal}; _} -> true
          | _ -> false)
        | _ -> false)
      | None -> false
    in
    let rel_spec = msg_spec_of_ident relative_msg in
    let declared =
      if msg = relative_msg then
        if init_offset = gap - 1 then Printf.sprintf "@#%d" gap else Printf.sprintf "@#%d~%d" init_offset gap
      else if gap = 0 then Printf.sprintf "@#%s" rel_spec.msg
      else Printf.sprintf "@#%s+%d" rel_spec.msg gap
    in
    let every n = if n = 1 then "every cycle" else "every " ^ EventGraphOps.cycles_phrase n in
    let at_most n =
      if n >= GraphAnalysis.event_distance_max then "an unbounded number of cycles"
      else EventGraphOps.cycles_phrase n
    in
    (* if msg is an action at this event, obtain until *)
    if config.verbose then (
      Printf.eprintf "Checking sync mode %s %s %b %b\n" msg relative_msg self_check other_check
    );

    let has_msg msg ev =
      let res = List.find_map (fun ac_span ->
        match ac_span.d with
        | ImmediateSend (msg', td) ->
          if (msg_ident msg') = msg then
            Some {d = {until = ev; ty = Send (msg', td)}; def_span = ac_span.def_span; action_event = ac_span.action_event; span = ac_span.span }
          else None
        | ImmediateRecv msg' ->
          if (msg_ident msg') = msg then
            Some {d = {until = ev; ty = Recv msg'}; def_span = ac_span.def_span; action_event = ac_span.action_event; span = ac_span.span }
          else None
        | _ -> None
      ) ev.actions in
      if Option.is_some res then res
      else (
        List.find_map
        (fun sa ->
          match sa.d.ty with
          | Send (msg', _)
          | Recv msg' -> (
            let mi' = msg_ident msg' in
            if mi' = msg then
              Some sa
            else
              None
          )
        ) ev.sustained_actions
      )
    in
    if self_check then (
      (* on the self side, check that adjacent events are no more than gap cycles apart,
         also check that the root to the first event is no more than init offset cycles apart
      *)
      (* check root *)
      let is_first = ref true in
      List.iter
        (fun ev ->
          match has_msg relative_msg ev with
          | Some sa ->
              if msg = relative_msg && !is_first then
                is_first := false (* skip the first msg (comes last) *)
              else (
                let slacks = GraphAnalysis.events_max_dist g.events lookup_message sa.d.until in
                (* mask out events that do not have the message *)
                List.iter (fun ev' ->
                  if has_msg msg ev' |> Option.is_none then
                    slacks.(ev'.id) <- GraphAnalysis.event_distance_max
                ) g.events;
                let min_weights = GraphAnalysis.event_min_among_succ g.events slacks in
                if config.verbose then (
                  Printf.eprintf "Relative to %d:\n" sa.d.until.id;
                  Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks;
                  Array.iteri (fun idx sl -> Printf.eprintf "Mw %d = %d\n" idx sl) min_weights
                );
                let actual = min_weights.(sa.d.until.id) in
                if actual > gap && not is_one_cycle_static_1_1 then (
                  let head =
                    if msg = relative_msg then [
                      Text (Printf.sprintf "%s can be more than %s apart" (quote_msg msg_spec) (EventGraphOps.cycles_phrase gap));
                      Label (None, sa.span, Printf.sprintf "after this %s, the next one can take %s" (describe_msg_use sa.d) (at_most actual));
                      Note (Printf.sprintf "%s is declared `%s`, so this side must perform it %s without waiting for the other side"
                        (quote_msg msg_spec) declared (every gap))
                    ] else [
                      Text (Printf.sprintf "%s can happen more than %s after %s"
                        (quote_msg msg_spec) (EventGraphOps.cycles_phrase gap) (quote_msg rel_spec));
                      Label (None, sa.span, Printf.sprintf "after this %s, %s can take %s" (describe_msg_use sa.d) (quote_msg msg_spec) (at_most actual));
                      Note (Printf.sprintf "%s is declared `%s`, so it must happen %s after %s"
                        (quote_msg msg_spec) declared (EventGraphOps.cycles_phrase gap) (quote_msg rel_spec))
                    ]
                  in
                  let unbounded =
                    if actual >= GraphAnalysis.event_distance_max then
                      [Note "a path through the loop waits for another message, which can take any number of cycles"]
                    else []
                  in
                  raise (LifetimeCheckError (head @ unbounded @ [
                    Help (Printf.sprintf "make every path through the loop reach the next %s in time, or declare it `@dyn` to use a handshake"
                      (quote_msg msg_spec))
                  ]))
                )
              )
          | None -> ()
        ) g.events;
      if msg = relative_msg then (
        let ev_root = (List.length g.events) - 1 |> List.nth g.events in
        assert(ev_root.source = (`Root None));
        let slacks = GraphAnalysis.events_max_dist g.events lookup_message ev_root in
        List.iter (fun ev' ->
          if has_msg relative_msg ev' |> Option.is_none then
            slacks.(ev'.id) <- GraphAnalysis.event_distance_max
        ) g.events;
        let min_weights = GraphAnalysis.event_min_among_succ g.events slacks in
        let actual = min_weights.(ev_root.id) in
        if actual > init_offset && not is_one_cycle_static_1_1 then
          raise (LifetimeCheckError [
            Text (Printf.sprintf "the first %s can happen later than cycle %d" (quote_msg msg_spec) init_offset);
            Label (None, g.thread_codespan, Printf.sprintf "the first one can take %s after the loop starts" (at_most actual));
            Note (Printf.sprintf "%s is declared `%s`, so this side must perform it first at cycle %d and then %s"
              (quote_msg msg_spec) declared init_offset (every gap));
            Help (Printf.sprintf "make every path reach the first %s within %s, or declare it `@dyn` to use a handshake"
              (quote_msg msg_spec) (EventGraphOps.cycles_phrase init_offset))
          ])
      )
    );
    if other_check then (
      (* if the other side is static, this side needs checking that the adjacent
      events are at least gap cycles apart; here we need to take into consideration
      the root event to ensure that the reference point is the beginning of a loop *)
      let has_msg_end msg ev =
        let res = List.find_map (fun ac_span ->
          match ac_span.d with
          | ImmediateSend (msg', _)
          | ImmediateRecv msg' ->
            if (msg_ident msg') = msg then
              Some ev
            else None
          | _ -> None
        ) ev.actions in
        if Option.is_some res then res
          else (
          match ev.source with
          | `Seq (ev', `Send msg')
          | `Seq (ev', `Recv msg') ->
            if (msg_ident msg') = msg then
              Some ev'
            else
              None
          | _ -> None
        )
      in
      let is_first = ref true in
      List.iter
        (fun ev ->
          match has_msg msg ev with
          | Some sa ->
            let slacks = GraphAnalysis.events_max_dist g.events lookup_message ev in
            if config.verbose then (
              Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks
            );
            let root_adjusted = msg = relative_msg && !is_first in
            List.iter (fun ev' ->
              if root_adjusted && (ev'.source = `Root None) then
                slacks.(ev'.id) <- slacks.(ev'.id) + init_offset - gap
              else if has_msg_end relative_msg ev' |> Option.is_none then
                slacks.(ev'.id) <- -event_distance_max
            ) g.events;
            is_first := false;
            let preds = GraphAnalysis.event_predecessors ev in
            let maxv = preds |> List.map (fun ev' -> slacks.(ev'.id))
              |> List.fold_left Int.max (-event_distance_max) in
            if maxv > -gap then (
              let from_root = root_adjusted
                && List.exists (fun ev' -> ev'.source = `Root None && slacks.(ev'.id) = maxv) preds in
              let msg_name = msg_spec.msg in
              raise (LifetimeCheckError (
                if from_root then
                  let start = -maxv + init_offset - gap in
                  [
                    Text (Printf.sprintf "the first %s can happen too early for the other side" (quote_msg msg_spec));
                    Label (None, sa.span, Printf.sprintf "this %s can happen %s after the loop starts" (describe_msg_use sa.d) (EventGraphOps.cycles_phrase start));
                    Note (Printf.sprintf "the other side is declared `%s`: it performs `%s` first at cycle %d and then %s"
                      declared msg_name init_offset (every gap));
                    Help (Printf.sprintf "wait until cycle %d before the first %s, e.g. with `cycle %d`" init_offset (describe_msg_use sa.d) (init_offset - start))
                  ]
                else if msg = relative_msg then [
                  Text (Printf.sprintf "%s can happen more often than the other side allows" (quote_msg msg_spec));
                  Label (None, sa.span, Printf.sprintf "this %s can happen again %s after the previous one" (describe_msg_use sa.d) (EventGraphOps.cycles_phrase (-maxv)));
                  Note (Printf.sprintf "the other side is declared `%s`: it performs `%s` only %s"
                    declared msg_name (every gap));
                  Help (Printf.sprintf "wait at least %s between two of them, e.g. with `cycle %d`" (EventGraphOps.cycles_phrase gap) (gap + maxv))
                ] else [
                  Text (Printf.sprintf "%s can happen less than %s after %s" (quote_msg msg_spec) (EventGraphOps.cycles_phrase gap) (quote_msg rel_spec));
                  Label (None, sa.span, Printf.sprintf "this %s can happen %s after %s" (describe_msg_use sa.d) (EventGraphOps.cycles_phrase (-maxv)) (quote_msg rel_spec));
                  Note (Printf.sprintf "the other side is declared `%s`: it performs `%s` %s after `%s`"
                    declared msg_name (EventGraphOps.cycles_phrase gap) rel_spec.msg);
                  Help (Printf.sprintf "wait at least %s after %s" (EventGraphOps.cycles_phrase gap) (quote_msg rel_spec))
                ]))
            )
          | None -> ()
        ) (List.rev g.events)
   )
  in
  Utils.StringMap.iter check_msg_sync_mode !msg_to_check;


