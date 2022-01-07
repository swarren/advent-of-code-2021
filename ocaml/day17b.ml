type vector = {x:int; y:int}
let vector_printer {x; y} =
  Format.sprintf "vector{x:%d; y:%d}" x y

type target = {xleft:int; xright:int; ybottom:int; ytop:int}
let target_printer {xleft; xright; ybottom; ytop} =
  Format.sprintf "vector{xleft:%d; xright:%d; ybottom:%d; ytop:%d}" xleft xright ybottom ytop

type result = Hit | MissRight | MissBelow | MissRightBelow
let result_printer = function
  | Hit -> "Hit"
  | MissRight -> "MissRight"
  | MissBelow -> "MissBelow"
  | MissRightBelow -> "MissRightBelow"

let read_parse_input fn =
  let fd = open_in fn in
    try 
      let line = input_line fd in
        close_in fd;
        Scanf.sscanf line "target area: x=%d..%d, y=%d..%d" (fun xl xr yb yt -> {xleft=xl; xright=xr; ybottom=yb; ytop=yt})
    with e ->
      close_in_noerr fd;
      raise e

let fire target velocity =
  let sign x =
    if x == 0 then 0
    else if velocity.x < 0 then -1
    else 1
  in let step coord velocity =
    let coord' = {x=coord.x + velocity.x; y=coord.y + velocity.y}
    in let velocity' = {x=velocity.x - sign velocity.x; y=velocity.y - 1}
    in (coord', velocity')
  in let rec steps target maxY coord velocity =
    let (coord', velocity') = step coord velocity
    in let isRightOfRight = coord'.x > target.xright
    in let isAtOrRightOfLeft = coord'.x >= target.xleft
    in let isBelowBottom = coord'.y < target.ybottom
    in let isAtOrBelowTop = coord'.y <= target.ytop
    in let maxY' = max maxY coord'.y 
    in if isRightOfRight && isBelowBottom
       then (MissRightBelow, maxY')
       else if isRightOfRight
            then (MissRight, maxY')
            else if isBelowBottom
                 then (MissBelow, maxY')
                 else if isAtOrRightOfLeft && isAtOrBelowTop
                      then (Hit, maxY')
                      else steps target maxY' coord' velocity'
  in steps target 0 {x=0; y=0} velocity

let target = read_parse_input "../input/day17.txt"
let numHits = ref 0
let () = for vx = 1 to target.xright do
  for vy = target.ybottom to 1000 do
    let velocity = {x=vx; y=vy}
    in let (result, maxY) = fire target velocity
    in (* let () = *) numHits := if result == Hit then !numHits + 1 else !numHits
    (*
    in let () = print_endline (vector_printer velocity)
    in let () = print_endline (result_printer result)
    in print_endline (string_of_int maxY)
    *)
  done
done
let () = print_endline (string_of_int !numHits)
