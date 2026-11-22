open Lang

module DTypeCheck = struct
  let bits n = if n = 1 then "1 bit" else Printf.sprintf "%d bits" n

  let size typedefs macro_defs span dtype =
    Except.with_span span (fun () ->
      try TypedefMap.data_type_size typedefs macro_defs dtype
      with Failure _ | Not_found | Invalid_argument _ ->
        raise (Except.TypeError [
          Text (Printf.sprintf "cannot determine the size of `%s`" (string_of_data_type dtype))
        ]))

  (* Phrases naming where a type check happens, completing "mismatched types ...". *)
  let fmt_assign lval = Printf.sprintf "in assignment to `%s`" (string_of_lvalue lval)
  let fmt_func_arg func_name arg_name = Printf.sprintf "for argument `%s` of `%s`" arg_name func_name
  let fmt_binop op = Printf.sprintf "in `%s`" (binop_symbol op)
  let fmt_send (msg_spec : message_specifier) = Printf.sprintf "in message `%s.%s`" msg_spec.endpoint msg_spec.msg
  let fmt_record_field field_name = Printf.sprintf "for field `%s`" field_name
  let fmt_let_binding ident = Printf.sprintf "for `%s`" ident
  let fmt_let = "in `let`"

  let check ~typedefs ~macro_defs ~context ~label ~expected ~got ~span ~weak_mode =
    if expected = got then
      ()
    else
      let exp_s = string_of_data_type expected and got_s = string_of_data_type got in
      let label = match label with
        | Some l -> l
        | None -> Printf.sprintf "expected `%s`, found `%s`" exp_s got_s
      in
      let se = size typedefs macro_defs span expected
      and sg = size typedefs macro_defs span got in
      let head = [Except.Text ("mismatched types " ^ context); Except.Label (None, span, label)] in
      if se <> sg then
        raise (Except.TypeError (head @ [
          Note (Printf.sprintf "`%s` is %s wide but `%s` is %s wide" exp_s (bits se) got_s (bits sg))
        ]))
      else if weak_mode then
        Except.warn (head @ [
          Note (Printf.sprintf "both types are %s wide, so the value is converted bit for bit" (bits se))
        ])
      else
        raise (Except.TypeError (head @ [
          Note (Printf.sprintf "both types are %s wide" (bits se));
          Help (Printf.sprintf "convert the value with a cast, e.g. `<(value) :: %s>`" exp_s)
        ]))
end

let check_dtype ?label context dtype1 dtype2 span _file_name allow typedefs macro_defs =
  match dtype1 with
  | Some dt1 ->
    DTypeCheck.check
      ~typedefs ~macro_defs
      ~context ~label ~expected:dt1 ~got:dtype2
      ~span ~weak_mode:allow
  | None -> ()
