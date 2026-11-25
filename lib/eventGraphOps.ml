open EventGraph


let lifetime_const current = {live = current; dead = [(current, `Eternal)]}
let lifetime_immediate current = {live = current; dead = [(current, `Cycles 1)]}

let full_reg_range regname size =
  {
    subreg_name = regname;
    subreg_range_interval = (Const 0, size)
  }

let sub_reg_range regname offset len =
  {
    subreg_name = regname;
    subreg_range_interval = (Const offset, len)
  }
  
let subreg_ranges_possibly_intersect r1 r2 =
  r1.subreg_name = r2.subreg_name &&
    (match fst r1.subreg_range_interval, fst r2.subreg_range_interval with
    | Const n1, Const n2 ->
      let end1 = n1 + (snd r1.subreg_range_interval)
      and end2 = n2 + (snd r2.subreg_range_interval) in
      end2 > n1 && end1 > n2
    | _ -> true
    )

let print_graph (g: event_graph) =
  List.iter (fun ev ->
    (match ev.origin with
     | Some o -> Printf.eprintf "> %d is the %s on line %d\n" ev.id o.what o.where.st.pos_lnum
     | None -> ());
    match ev.source with
    | `Later (e1, e2) -> Printf.eprintf "> %d: later %d %d\n" ev.id e1.id e2.id
    | `Seq (ev', a) ->
      let c = match a with
      | `Cycles n -> Printf.sprintf "C %d" n
      | `Send _ -> "S"
      | `Recv _ -> "R"
      in
      Printf.eprintf "> %d: seq %d %s\n" ev.id ev'.id c
    | `Branch (ev', _) -> Printf.eprintf "> %d: branch %d\n" ev.id ev'.id
    | `Root None -> Printf.eprintf "> %d: root\n" ev.id
    | `Root (Some (ev', br_side_info)) ->
        Printf.eprintf "> %d: branch-root %d %d\n" ev.id br_side_info.branch_side_sel ev'.id
  ) g.events

let print_dot_graph g out =
  Printf.fprintf out "// BEGIN GRAPH IN DOT FORMAT\n";
  Printf.fprintf out "// can render to PDF with 'dot -Tpdf -O <filename>'\n";
  Printf.fprintf out "digraph {\n";
  let ev_node_name ev = Printf.sprintf "event%d" ev.id in
  let print_edge e1 e2 label =
    Printf.fprintf out "  %s -> %s [label = \"%s\"];\n"
      (ev_node_name e1)
      (ev_node_name e2)
      label
  in
  List.iter (fun ev ->
    if ev.is_recurse then
      Printf.fprintf out "  %s [peripheries=2];\n" @@ ev_node_name ev;
    match ev.source with
    | `Later (e1, e2) ->
      print_edge e1 ev "L";
      print_edge e2 ev "L"
    | `Seq (ev', a) ->
      let label = match a with
      | `Cycles n -> Printf.sprintf "#%d" n
      | `Send _ -> "S"
      | `Recv _ -> "R"
      in
      print_edge ev' ev label
    | `Branch (_, br_info) ->
      List.iter (fun e ->
        print_edge e ev "B"
      ) br_info.branches_val
    | `Root None -> ()
    | `Root (Some (ev', br_side_info)) ->
      print_edge ev' ev @@ string_of_int br_side_info.branch_side_sel
  ) g.events;
  Printf.fprintf out "}\n";
  Printf.fprintf out "// END GRAPH IN DOT FORMAT\n"

let string_of_actions action = 
  match action with
  | DebugPrint _ -> "DebugPrint"
  | DebugFinish -> "DebugFinish"
  | RegAssign _ -> "RegAssign"
  | ImmediateRecv _ -> "ImmediateRecv"
  | ImmediateSend _ -> "ImmediateSend"

let cycles_phrase n = if n = 1 then "1 cycle" else Printf.sprintf "%d cycles" n

let rec describe_event (ev : event) =
  match ev.origin with
  | Some o -> Printf.sprintf "the %s on line %d" o.what o.where.st.pos_lnum
  | None ->
    match ev.source with
    | `Root None -> "the start of the loop iteration"
    | `Root (Some _) -> "the start of a branch"
    | `Branch _ -> "the end of a branch"
    | `Seq (ev', `Cycles n) -> Printf.sprintf "%s after %s" (cycles_phrase n) (describe_event ev')
    | `Seq (_, `Send m) -> Printf.sprintf "the `send %s.%s`" m.endpoint m.msg
    | `Seq (_, `Recv m) -> Printf.sprintf "the `recv %s.%s`" m.endpoint m.msg
    | `Later _ ->
      let rec leaves (e : event) =
        match e.origin, e.source with
        | None, `Later (e1, e2) -> leaves e1 @ leaves e2
        | _ -> [e]
      in
      let names = leaves ev
        |> List.sort_uniq (fun (a : event) (b : event) -> compare a.id b.id)
        |> List.map describe_event
      in
      match List.rev names with
      | [] -> "an event"
      | [n] -> n
      | [b; a] -> Printf.sprintf "both %s and %s" a b
      | last :: rest -> Printf.sprintf "all of %s and %s" (String.concat ", " (List.rev rest)) last

let describe_pat_after (pat : Lang.delay_pat) at =
  match pat with
  | `Eternal -> "forever"
  | `Cycles 0 -> at
  | `Cycles n -> Printf.sprintf "%s after %s" (cycles_phrase n) at
  | `Message_with_offset (m, o, plus) ->
    let next = Printf.sprintf "the next `%s.%s` after %s" m.endpoint m.msg at in
    if o = 0 then next
    else Printf.sprintf "%s %s %s" (cycles_phrase o) (if plus then "after" else "before") next
