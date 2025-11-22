(* day 14*)

(* Type pour une position *)
type position = int * int;;

(* Utiliser un Set pour stocker les positions occupées *)
module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)
;;

(* État de la grille *)
type grid = {
  rocks: PosSet.t;        (* Positions des rochers *)
  _sand: PosSet.t;         (* Positions du sable *)
  max_y: int;             (* Plus basse position de rocher *)
}
;;


(* Parse une position "x,y" *)
let parse_position s =
  match String.split_on_char ',' s with
  | [x; y] -> (int_of_string (String.trim x), int_of_string (String.trim y))
  | _ -> failwith ("Invalid position: " ^ s)
;;

(* Parse une ligne par exemple : "498,4 -> 498,6 -> 496,6" *)
let parse_line line =
  String.split_on_char ' ' line  (* Split par espace au lieu de '>' *)
  |> List.map String.trim
  |> List.filter (fun s -> s <> "" && s <> "-" && s <> "->")  (* Filtre "->" *)
  |> List.map parse_position
;;

(* Génère toutes les positions entre deux points (ligne horizontale ou verticale) *)
let positions_between (x1, y1) (x2, y2) =
  if x1 = x2 then
    (* Ligne verticale *)
    let min_y = min y1 y2 in
    let max_y = max y1 y2 in
    List.init (max_y - min_y + 1) (fun i -> (x1, min_y + i))
  else if y1 = y2 then
    (* Ligne horizontale *)
    let min_x = min x1 x2 in
    let max_x = max x1 x2 in
    List.init (max_x - min_x + 1) (fun i -> (min_x + i, y1))
  else
    failwith "Diagonal lines not supported"
;;

(* Génère toutes les positions de rochers pour un chemin *)
let rocks_from_path positions =
  let rec loop acc = function
    | [] | [_] -> acc
    | p1 :: p2 :: rest ->
        let segment_rocks = positions_between p1 p2 in
        loop (segment_rocks @ acc) (p2 :: rest)
  in
  loop [] positions
;;

(* Parse toutes les lignes et retourne un Set de positions de rochers *)
let parse_rocks lines =
  List.fold_left (fun acc line ->
    let path = parse_line line in
    let rocks = rocks_from_path path in
    List.fold_left (fun set pos -> PosSet.add pos set) acc rocks
  ) PosSet.empty lines
;;

(* Trouve le y maximum (le plus bas) *)
let find_max_y rocks =
  PosSet.fold (fun (_, y) max_y -> max y max_y) rocks 0
;;

(* Initialise la grille *)
let init_grid lines =
  let rocks = parse_rocks lines in
  let max_y = find_max_y rocks in
  {
    rocks = rocks;
    _sand = PosSet.empty;
    max_y = max_y;
  }
;;

(* Vérifie si une position est occupée *)
let is_occupied pos rocks sand =
  PosSet.mem pos rocks || PosSet.mem pos sand
;;

(* Simule la chute d'un grain de sable *)
(* Retourne None si le sable tombe dans le vide, Some pos sinon *)
let simulate_sand_grain rocks sand max_y =
  let start = (500, 0) in
  
  let rec fall (x, y) =
    (* Si on dépasse max_y, le sable tombe dans le vide *)
    if y > max_y then None
    else
      let down = (x, y + 1) in
      let down_left = (x - 1, y + 1) in
      let down_right = (x + 1, y + 1) in
      
      if not (is_occupied down rocks sand) then
        fall down
      else if not (is_occupied down_left rocks sand) then
        fall down_left
      else if not (is_occupied down_right rocks sand) then
        fall down_right
      else
        (* Le sable s'immobilise *)
        Some (x, y)
  in
  
  fall start
;;

(* Simule la chute du sable jusqu'à ce qu'il tombe dans le vide (partie 1) *)
let simulate_part1 grid =
  let rec loop sand count =
    match simulate_sand_grain grid.rocks sand grid.max_y with
    | None -> count  (* Le sable tombe dans le vide *)
    | Some pos ->
        let new_sand = PosSet.add pos sand in
        loop new_sand (count + 1)
  in
  loop PosSet.empty 0
;;

(* Simule la chute d'un grain avec un sol à floor_y *)
let simulate_sand_grain_with_floor rocks sand floor_y =
  let start = (500, 0) in
  
  let rec fall (x, y) =
    (* Si on atteint le sol, on s'immobilise *)
    if y + 1 = floor_y then Some (x, y)
    else
      let down = (x, y + 1) in
      let down_left = (x - 1, y + 1) in
      let down_right = (x + 1, y + 1) in
      
      if not (is_occupied down rocks sand) then
        fall down
      else if not (is_occupied down_left rocks sand) then
        fall down_left
      else if not (is_occupied down_right rocks sand) then
        fall down_right
      else
        (* Le sable s'immobilise *)
        Some (x, y)
  in
  
  fall start
;;

(* Simule avec le sol (partie 2) *)
let simulate_part2 grid =
  let floor_y = grid.max_y + 2 in
  let rec loop sand count =
    let result = simulate_sand_grain_with_floor grid.rocks sand floor_y in
    match result with
    | Some pos ->
        let new_sand = PosSet.add pos sand in
        let new_count = count + 1 in
        (* Si on vient de bloquer la source, on s'arrête *)
        if pos = (500, 0) then new_count
        else loop new_sand new_count
    | None -> count  (* Ne devrait jamais arriver avec un sol *)
  in
  loop PosSet.empty 0
;;

let solve_part1 lines =
  let grid = init_grid lines in
  simulate_part1 grid
;;

let solve_part2 lines =
  let grid = init_grid lines in
  simulate_part2 grid
;;

let () =
  let lines = Utils.read_lines "input/day14.txt" in
  let result_part1 = solve_part1 lines in
  let result_part2 = solve_part2 lines in
  
  print_endline ("day14, part 1 : " ^ string_of_int result_part1);
  print_endline ("day14, part 2 : " ^ string_of_int result_part2);
;;