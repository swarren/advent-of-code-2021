type snailnum = Pair of snailnum * snailnum | Int of int
let rec snailnum_printer sn =
  match sn with
    | Int n -> string_of_int n
    | Pair (l, r) -> Format.sprintf "[%s,%s]" (snailnum_printer l) (snailnum_printer r)

let parse_digit s idx =
  (idx + 1, Int (Char.code (String.get s idx) - Char.code '0'))

let rec parse_pair s idx =
    let after_l_bracket = idx + 1 in
    let (after_l_sn, l_sn) = parse_sn s after_l_bracket in
    let after_comma = after_l_sn + 1 in
    let (after_r_sn, r_sn) = parse_sn s after_comma in
    let after_r_bracket = after_r_sn + 1 in
    (after_r_bracket, Pair (l_sn, r_sn))
and parse_sn s idx =
  match (String.get s idx) with
    | '[' -> parse_pair s idx
    | _ -> parse_digit s idx

let parse_line s =
  let (idx', sn) = parse_sn s 0 in
  sn

let read_parse_input fn =
  let lines = ref [] in
  let chan = open_in fn in
  try
    while true; do
      lines := parse_line (input_line chan) :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let rec snailnum_add_rightmost sn nadd =
  match sn with
    | Int n -> Int (n + nadd)
    | Pair (l, r) -> Pair (l, snailnum_add_rightmost r nadd)

let rec snailnum_add_leftmost sn nadd =
  match sn with
    | Int n -> Int (n + nadd)
    | Pair (l, r) -> Pair (snailnum_add_leftmost l nadd, r)

exception SnailNumFailure of string

let snailnum_to_int sn =
  match sn with
  | Int n -> n
  | Pair (l, r) -> raise (SnailNumFailure "snailnum_to_int called on Pair")

let snailnum_reduce_explode sn =
  let rec snailnum_reduce_explode' sn depth =
    match sn with
    | Int n -> (sn, false, Option.None, Option.None)
    | Pair (l, r) ->
      if depth == 4
      then (Int 0, true, Option.Some (snailnum_to_int l), Option.Some (snailnum_to_int r))
      else
        let (l', lmodified', llwork', lrwork') = snailnum_reduce_explode' l (depth + 1) in
        let (r', rmodified', rlwork', rrwork') =
          if Option.is_some lrwork'
          then (snailnum_add_leftmost r (Option.get lrwork'), true, Option.None, Option.None)
          else
            if lmodified'
            then (r, false, Option.None, Option.None)
            else snailnum_reduce_explode' r (depth + 1)
        in
        let l'' =
          if Option.is_some rlwork'
          then snailnum_add_rightmost l' (Option.get rlwork')
          else l'
        in
        let modified' = lmodified' || rmodified' in
        (Pair (l'', r'), modified', llwork', rrwork')
  in
  let (sn', modified, lwork, rwork) = snailnum_reduce_explode' sn 0 in
  (sn', modified)

let rec snailnum_reduce_split sn =
  match sn with
    | Int n ->
      if n < 10
      then (sn, false)
      else (Pair (Int (n / 2), Int (n / 2 + n mod 2)), true)
    | Pair (l, r) ->
      let (l', l_modified) = snailnum_reduce_split l in
      let (r', r_modified) =
        if l_modified
        then (r, false)
        else snailnum_reduce_split r
      in
      let modified = l_modified || r_modified in
      if modified
      then (Pair (l', r'), modified)
      else (sn, false)

let snailnum_reduce_either sn =
  let (sn_explode, modified_explode) = snailnum_reduce_explode sn in
  let (sn_split, modified_split) =
    if modified_explode
    then (sn_explode, modified_explode)
    else snailnum_reduce_split sn
  in
  (sn_split, modified_split)

let rec snailnum_reduce sn =
  (* let () = print_string "> "; print_endline (snailnum_printer sn) in *)
  let (sn', modified) = snailnum_reduce_either sn in
  if modified
  then snailnum_reduce sn'
  else sn

let snailnum_add a b =
    snailnum_reduce (Pair (a, b))

let rec snailnum_magnitude sn =
  match sn with
    | Int n -> n
    | Pair (l, r) -> (3 * snailnum_magnitude l) + (2 * snailnum_magnitude r)

let snailnum_add_debug a b =
  let () = print_string "  "; print_endline (snailnum_printer a) in
  let () = print_string "+ "; print_endline (snailnum_printer b) in
  let sn = snailnum_add a b in
  let () = print_string "= "; print_endline (snailnum_printer sn) in
  let () = print_endline "" in
  sn

let pairs l =
  let rec pairs'' l e skipix ix =
    match l with
    | [] -> []
    | h::t ->
      let sub = pairs'' t e skipix (ix + 1) in
      if skipix == ix
      then sub
      else (e, h) :: sub
  in
  let rec pairs' l le ix =
    match le with
    | [] -> []
    | h::t -> (pairs'' l h ix 0) @ (pairs' l t (ix + 1))
  in
  pairs' l l 0

let input = read_parse_input "../input/day18.txt"
let sums = List.map (fun (a, b) -> snailnum_add a b) (pairs input)
let magnitudes = List.map snailnum_magnitude sums
let answer = List.fold_left (fun acc x -> max acc x) 0 magnitudes
let () = print_endline (string_of_int answer)
