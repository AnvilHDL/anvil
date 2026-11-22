
type t = {
  buffer : bytes;
  mutable pos : int64;
  len : int64;
}

let file_data_cache : (string, bytes) Hashtbl.t = Hashtbl.create 16

(* Opens a file and reads its entire content into memory, caching it for future use.
   If the filename is "-", reads from standard input.
   The provided function [f] is then called with an in-memory channel containing the file's content. *)
let with_open filename f =
  let buffer =
    match filename with
    | "-" ->
        let contents =
          if Hashtbl.mem file_data_cache filename then
            Hashtbl.find file_data_cache filename
          else
            let ic = In_channel.stdin in
            let bytes = In_channel.input_all ic |> Bytes.of_string in
            let _ = Hashtbl.add file_data_cache filename bytes in
            bytes
        in
        contents
    | filename ->
        if Hashtbl.mem file_data_cache filename then
          Hashtbl.find file_data_cache filename
        else
          let ic = In_channel.open_bin filename in
          let bytes = In_channel.input_all ic |> Bytes.of_string in
          let _ = In_channel.close ic in
          let _ = Hashtbl.add file_data_cache filename bytes in
          bytes
  in
  let len = Int64.of_int(Bytes.length buffer) in
  let t = { buffer; pos = 0L; len } in
  f t

(* cached file contents *)
let cached_contents filename = Hashtbl.find_opt file_data_cache filename


(* Opens a file similarly to [with_open], but allows aliasing the filename for caching purposes.
   This is useful when, e.g., reading from standard input, but wanting to associate it with a filename or base path.
   If [aliased_filename] is identical to [source_filename], it makes no difference to executing `with_open`. *)
let with_open_aliased source_filename aliased_filename f =
  let _ = if source_filename <> aliased_filename then
    let _ = with_open source_filename (fun _ -> ()) in
    if Hashtbl.mem file_data_cache source_filename then (
      if aliased_filename = "-" then ()
      else
        let contents = Hashtbl.find file_data_cache source_filename in
        Hashtbl.replace file_data_cache aliased_filename contents
    )
  else () in
  with_open aliased_filename f


(* Reads up to [len] bytes from the in-memory channel into [dst] starting at [dst_pos].
   Returns the number of bytes actually read. *)
let input t dst dst_pos len =
  let avail = Int64.sub t.len t.pos in
  let to_read = Int64.min avail len in
  Bytes.blit t.buffer (Int64.to_int t.pos) dst (Int64.to_int dst_pos) (Int64.to_int to_read);
  t.pos <- Int64.add t.pos to_read;
  to_read

(* Reads a line from the in-memory channel, returning it as a string option.
   Lines end at "\n", "\r\n" or "\r", as in the lexer; the line ending is not
   included in the returned string.
   Returns [None] if the end of the channel is reached. *)
let input_line t =
  if t.pos >= t.len then None
  else
    let len = Int64.to_int t.len in
    let start = Int64.to_int t.pos in
    let rec find_eol i =
      if i >= len then i
      else match Bytes.get t.buffer i with
        | '\n' | '\r' -> i
        | _ -> find_eol (i + 1)
    in
    let eol = find_eol start in
    let next =
      if eol >= len then eol
      else if Bytes.get t.buffer eol = '\r' && eol + 1 < len && Bytes.get t.buffer (eol + 1) = '\n' then eol + 2
      else eol + 1
    in
    t.pos <- Int64.of_int next;
    Some (Bytes.sub_string t.buffer start (eol - start))

(* Seeks to a specific position in the channel. *)
let seek t pos =
  if pos < 0L || pos > t.len then invalid_arg "InChannelCacheableAliasable.seek";
  t.pos <- pos

(* Returns the current position in the channel. *)
let pos t = t.pos

(* Returns the total length of the channel contents. *)
let length t = t.len
