open ParamEnv
open Lang

let rec concretise_params int_env type_env params =
  List.map (fun p ->
    match p with
    | IntParamValue _ -> p
    | TypeParamValue dtype ->
      (
        match dtype with
        | `Named (ident, []) ->
          (* this is just an identifier, might also be an int param *)
          (
            match concretise_and_get int_env (Param ident) with
            | Some n -> IntParamValue n
            | None -> TypeParamValue (concretise_dtype_params int_env type_env dtype)
          )
        | _ -> TypeParamValue (concretise_dtype_params int_env type_env dtype)
      )
  ) params
and concretise_dtype_params int_env type_env (dtype : data_type) : data_type =
  match dtype with
  | `Array (v, n) ->
    let base_type = concretise_dtype_params int_env type_env v in
    let size = Option.value (concretise int_env n) ~default:n in
    `Array (base_type, size)
  | `Named (ident, params) ->
    if params = [] then
      let t = Param ident in
      (
        match concretise_and_get type_env t with
        | Some v -> v
        | None -> dtype
      )
    else
      let params' = concretise_params int_env type_env params in
      `Named (ident, params')
  | `Record fields ->
    let fields' = List.map (fun ({d = (field_ident, field_dtype); _} as node) ->
      let new_data = (field_ident, concretise_dtype_params int_env type_env field_dtype) in
      {
        node with d = new_data
      }
    ) fields
    in
    `Record fields'
  | `Variant (dtype_opt, variants) ->
    let variants' = List.map
             (fun ({d = (var_ident, var_dtype_opt, var_val_opt); _} as node) ->
               { node with d = (
                var_ident,
                Option.map (concretise_dtype_params int_env type_env) var_dtype_opt,
                var_val_opt
              )})
            variants
            in
    let dtype_opt' = Option.map (concretise_dtype_params int_env type_env) dtype_opt in
    `Variant (dtype_opt', variants')
  | `Tuple elems_dtypes ->
    let elems_dtypes' = List.map (concretise_dtype_params int_env type_env) elems_dtypes in
    `Tuple elems_dtypes'
  | _ -> dtype

let string_of_param (p : param) =
  p.param_name ^ (match p.param_ty with IntParam -> " : int" | TypeParam -> " : type")

let check_param_count context params param_values =
  let expected = List.length params
  and actual = List.length param_values in
  if expected <> actual then (
    let parameter_word = if expected = 1 then "parameter" else "parameters" in
    let declared =
      if params = [] then Printf.sprintf "`%s` has no parameters" context
      else Printf.sprintf "`%s` is declared with `<%s>`" context
          (String.concat ", " (List.map string_of_param params))
    in
    raise (Except.TypeError [
      Text (Printf.sprintf "`%s` takes %d %s but %d %s given"
        context expected parameter_word actual (if actual = 1 then "was" else "were"));
      Note declared
    ])
  )

let build_param_envs context param_values params =
  check_param_count context params param_values;
  let int_env = create_env ()
  and type_env = create_env () in
  let vals_s = List.to_seq param_values
  and params_s = List.to_seq params in
  Seq.zip vals_s params_s
    |> Seq.iter
      (fun n ->
        let (v, p) = n in
        match v, p.param_ty with
        | IntParamValue n, IntParam ->
          add_value p.param_name n int_env
        | TypeParamValue t, TypeParam ->
          add_value p.param_name t type_env
        | _ ->
          let kind = match p.param_ty with IntParam -> "an integer" | TypeParam -> "a type" in
          raise (Except.TypeError [
            Text (Printf.sprintf "parameter `%s` of `%s` must be %s" p.param_name context kind);
            Note (Printf.sprintf "`%s` is declared with `<%s>`" context
              (String.concat ", " (List.map string_of_param params)))
          ])
      );
  (int_env, type_env)

let concretise_proc _context param_values proc =
  if proc.params = [] then
    proc
  else (
    let (int_param_env, type_param_env) = build_param_envs proc.name param_values proc.params in
    let args = List.map
      (fun ({d = a; def_span; action_event; span} : endpoint_def ast_node) ->
        let params = concretise_params int_param_env type_param_env a.channel_params in
        {d = {a with channel_params = params}; def_span; action_event; span}
      ) proc.args in
    (
      match proc.body with
      | Extern _ -> proc
      | Native body ->
        let regs = List.map
          (fun {d = r; def_span; action_event; span} -> {d = {r with d_type = concretise_dtype_params int_param_env type_param_env r.d_type}; def_span; action_event; span})
          body.regs in
        let spawns = List.map
          (fun {d = sp; def_span; action_event; span} ->
            let compile_params = concretise_params int_param_env type_param_env sp.compile_params in
            {d = {sp with compile_params}; def_span; action_event; span}
          )
          body.spawns in
        let channels = List.map
          (fun {d = ch; def_span; action_event; span} ->
            let params = concretise_params int_param_env type_param_env ch.channel_params in
            {d = {ch with channel_params = params}; def_span; action_event; span}
          )
          body.channels in
        {proc with args; body = Native {body with regs; spawns; channels}}
    )
  )

let concretise_dtype context params param_values =
  let (int_param_env, type_param_env) = build_param_envs context param_values params in
  concretise_dtype_params int_param_env type_param_env

let concretise_message context params param_values (msg : Lang.message_def) =
  let (int_param_env, type_param_env) = build_param_envs context param_values params in
  let sig_types = List.map
    (fun (stype : Lang.sig_type_chan_local) ->
      {stype with dtype = concretise_dtype_params int_param_env type_param_env stype.dtype}
    )
    msg.sig_types
  in
  {msg with sig_types}


let concretise_array_dimm context params param_values (dimm : Lang.array_dimensions) : array_dimm_concrete =
  let (int_param_env, _) = build_param_envs context param_values params in
  let mappings = ParamEnv.map_list (fun k v ->
    Printf.sprintf "%s : %d\n" k v
  ) int_param_env in
  let mappings = String.concat "" mappings in

  let get_concrete_params n = 
    match ParamEnv.get_opt n with
    | Some v -> v
    | None -> failwith "Unreachable : Please report bug"
  in
  let rec concretise_dimm dimm =
    match dimm with
    | Lang.OneDim n ->
      let n' = match concretise_and_get int_param_env n with
        | Some v -> ParamEnv.Concrete v
        | None ->
          Printf.eprintf "[ERROR] Failed to concretise array dimension parameter: %s | Current Mappings = %s\n"
            (match n with
             | ParamEnv.Concrete i -> Printf.sprintf "Concrete(%d)" i
             | ParamEnv.Param p -> Printf.sprintf "Param(%s)" p)
            mappings;
          failwith "Array dimension concretisation failed"
      in
      Lang.OneDimmension (get_concrete_params n')
    | Lang.MultiDim (n, rest) ->
      let rest' =  concretise_dimm rest in
      let n' = match concretise_and_get int_param_env n with
        | Some v -> ParamEnv.Concrete v
        | None ->
          Printf.eprintf "[ERROR] Failed to concretise array dimension parameter: %s | Current Mappings = %s\n"
            (match n with
             | ParamEnv.Concrete i -> Printf.sprintf "Concrete(%d)" i
             | ParamEnv.Param p -> Printf.sprintf "Param(%s)" p)
            mappings;
          failwith "Array dimension concretisation failed"
      in
      Lang.MultiDimmension ((get_concrete_params n'), rest')
  in
  concretise_dimm dimm
