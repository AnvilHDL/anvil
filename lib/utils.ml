module StringMap = Map.Make(String)
type 'a string_map = 'a StringMap.t

module StringSet = Set.Make(String)
type string_set = StringSet.t

module IntSet = Set.Make(Int)
type int_set = IntSet.t

(** min n. 2^n >= v *)
let int_log2 (v : int) : int =
  let n = ref 1
  and c = ref 0 in
  while !n < v do
    n := !n * 2;
    c := !c + 1
  done;
  !c

(* TODO: better error messages *)
let list_match_reorder (order : string list)
                       (data : (string * 'a) list) : 'a list option =
  if (List.length order) <> (List.length data) then
    None
  else
    let order_map = List.mapi (fun idx x -> (x, idx)) order |> List.to_seq |> StringMap.of_seq in
    try
      let data_ordered = List.map (fun (k, x) -> (StringMap.find k order_map, x)) data |>
          List.sort (fun (idx1, _) (idx2, _) -> idx1 - idx2) in
      let bad = ref false in
      List.iteri (fun idx (i, _) -> if idx <> i then bad := true) data_ordered;
      if !bad then
        None
      else
        Some (List.map snd data_ordered)
    with _ -> None

let list_unordered_join a b =
  if List.compare_lengths a b < 0 then
    a @ b
  else
    b @ a

let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let prev = Array.init (lb + 1) Fun.id and cur = Array.make (lb + 1) 0 in
  for i = 1 to la do
    cur.(0) <- i;
    for j = 1 to lb do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      cur.(j) <- min (min (prev.(j) + 1) (cur.(j - 1) + 1)) (prev.(j - 1) + cost)
    done;
    Array.blit cur 0 prev 0 (lb + 1)
  done;
  prev.(lb)

let closest_name name candidates =
  let limit = max 1 (String.length name / 3) in
  List.filter (fun c -> c <> name) candidates
  |> List.map (fun c -> (edit_distance name c, c))
  |> List.filter (fun (d, _) -> d <= limit)
  |> List.sort compare
  |> function
    | [] -> None
    | (_, c) :: _ -> Some c
