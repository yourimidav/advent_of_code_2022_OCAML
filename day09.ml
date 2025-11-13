(*day 09*)

type position = int * int;;

module PositionSet = Set.Make(struct
  type t = position
  let compare = compare
end);;

type direction = Up | Down | Left | Right;;
type instruction = { dir: direction; steps: int };;


let parse_direction = function
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Direction inconnue"
;;

let parse_instruction line =
  let dir_char = line.[0] in
  let steps = int_of_string (String.sub line 2 (String.length line - 2)) in
  { dir = parse_direction dir_char; steps }
;;

let move_head (x, y) = function
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)
;;

let update_tail (hx, hy) (tx, ty) =
  let dx = hx - tx in
  let dy = hy - ty in
  if abs dx <= 1 && abs dy <= 1 then (tx, ty)
  else
    let new_tx = tx + (if dx = 0 then 0 else dx / abs dx) in
    let new_ty = ty + (if dy = 0 then 0 else dy / abs dy) in
    (new_tx, new_ty)
;;

let process_instruction_1 head tail visited { dir; steps } =
  let rec loop head tail visited steps_left =
    if steps_left <= 0 then (head, tail, visited)
    else
      let new_head = move_head head dir in
      let new_tail = update_tail new_head tail in
      let new_visited = PositionSet.add new_tail visited in
      loop new_head new_tail new_visited (steps_left - 1)
  in
  loop head tail visited steps
;;

let solve_part1 lines =
  let instructions = List.map parse_instruction lines in
  let head = (0, 0) in
  let tail = (0, 0) in
  let visited = PositionSet.singleton tail in

  let rec loop head tail visited = function
    | [] -> PositionSet.cardinal visited
    | instr :: tl ->
        let new_head, new_tail, new_visited = process_instruction_1 head tail visited instr in
        loop new_head new_tail new_visited tl
  in
  loop head tail visited instructions
;;

let update_rope head rope =
  let rec update_knots prev_knot = function
    | [] -> []
    | knot :: tl ->
        let new_knot = update_tail prev_knot knot in
        new_knot :: update_knots new_knot tl
  in
  match rope with
  | [] -> []
  | first_knot :: tl ->
      let new_first_knot = update_tail head first_knot in
      new_first_knot :: update_knots new_first_knot tl
;;

let process_instruction_2 rope visited { dir; steps } =
  let rec loop rope visited steps_left =
    if steps_left <= 0 then (rope, visited)
    else
      let new_head = move_head (List.hd rope) dir in
      let new_rope = new_head :: update_rope new_head (List.tl rope) in
      let new_visited = PositionSet.add (List.hd (List.rev new_rope)) visited in
      loop new_rope new_visited (steps_left - 1)
  in
  loop rope visited steps
;;

let solve_part2 lines =
  let instructions = List.map parse_instruction lines in
  let initial_rope = List.init 10 (fun _ -> (0, 0)) in
  let visited = PositionSet.singleton (0, 0) in

  let rec loop rope visited = function
    | [] -> PositionSet.cardinal visited
    | instr :: tl ->
        let new_rope, new_visited = process_instruction_2 rope visited instr in
        loop new_rope new_visited tl
  in
  loop initial_rope visited instructions
;;

let () =
  let lines = Utils.read_lines "input/day09.txt" in 
  let result01 = solve_part1 lines in
  let result02 = solve_part2 lines in
  Printf.printf "Nombre de positions visitées par la queue : %d\n" result01;
  Printf.printf "Nombre de positions visitées par la queue : %d\n" result02;