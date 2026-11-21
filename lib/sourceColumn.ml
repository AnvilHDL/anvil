(** UTF-16 column of a position (LSP convention); byte column if uncached. *)
let utf16_column (p : Lexing.position) : int =
  let byte_col = p.pos_cnum - p.pos_bol in
  match InChannelCacheableAliasable.cached_contents p.pos_fname with
  | Some buf when byte_col > 0 && p.pos_bol >= 0 && p.pos_cnum <= Bytes.length buf ->
    let rec count i units =
      if i >= p.pos_cnum then units
      else
        let d = Bytes.get_utf_8_uchar buf i in
        let width = if Uchar.to_int (Uchar.utf_decode_uchar d) >= 0x10000 then 2 else 1 in
        count (i + Uchar.utf_decode_length d) (units + width)
    in
    count p.pos_bol 0
  | _ -> byte_col
