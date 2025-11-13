(* day 12 *)
(* j'ai choisi d'utiliser Breadth-First Search (BFS) car cela semblait approprié en terme de vitesse car toutes les arêtes ont le même coût *)
type position = int * int
type height = int
type grid = height array array

let filter_mapi f lst =
  let rec loop i acc = function
    | [] -> List.rev acc
    | hd :: tl ->
        match f i hd with
        | Some x -> loop (i + 1) (x :: acc) tl
        | None -> loop (i + 1) acc tl
  in
  loop 0 [] lst


let char_to_height letter =
  match letter with
  | 'S' -> 0
  | 'E' -> 25 
  | _ -> int_of_char letter - int_of_char 'a' 

(* Convertit une ligne en un tableau de hauteurs et cherche S/E *)
let parse_line (y: int) (line: string) : height array * position option * position option =
  let chars = String.to_seq line |> List.of_seq in
  let heights = List.map char_to_height chars |> Array.of_list in
  
  (* Cherche directement les caractères 'S' et 'E', pas leurs hauteurs *)
  let find_char target_char x c =
    if c = target_char then Some (x, y) else None
  in
  
  let start_pos = filter_mapi (fun x c -> find_char 'S' x c) chars in
  let goal_pos = filter_mapi (fun x c -> find_char 'E' x c) chars in
  
  let start = match start_pos with
    | [] -> None
    | hd :: _ -> Some hd
  in
  let goal = match goal_pos with
    | [] -> None
    | hd :: _ -> Some hd
  in
  (heights, start, goal)
;;

(* Convertit toutes les lignes en une grille et trouve S/E *)
let parse_grid (lines: string list) : grid * position * position =
  let parse_line_with_pos y line = parse_line y line in
  let lines_with_pos = List.mapi parse_line_with_pos lines in
  let grid: grid = List.map (fun (heights, _, _) -> heights) lines_with_pos |> Array.of_list in
  let starts = List.filter_map (fun (_, start, _) -> start) lines_with_pos in
  let goals = List.filter_map (fun (_, _, goal) -> goal) lines_with_pos in
  let start = match starts with
    | [pos] -> pos
    | [] -> failwith "Start position not found"
    | _ -> failwith "Multiple start positions found"
  in
  let goal = match goals with
    | [pos] -> pos
    | [] -> failwith "Goal position not found"
    | _ -> failwith "Multiple goal positions found"
  in
  (grid, start, goal)
;;

(* Fonction pour afficher la grille *)
let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun height ->
      let c =
        if height = 0 then 'S'
        else if height = 25 then 'E' 
        else char_of_int (height + int_of_char 'a')
      in
      print_char c
    ) row;
    print_newline ()
  ) grid
;;

(*maintenant le BFS *)
module PositionSet = Set.Make(struct
  type t = position
  let compare = compare
end)
;;
module PositionMap = Map.Make(struct
  type t = position
  let compare = compare
end)
;;

let bfs grid start goal =
  let height = Array.length grid in
  let width = Array.length grid.(0) in

  let get_neighbors (x, y) =
    [(x+1, y); (x-1, y); (x, y+1); (x, y-1)]
    |> List.filter (fun (nx, ny) ->
        nx >= 0 && nx < width && ny >= 0 && ny < height
    )
    |> List.filter (fun (nx, ny) ->
        let neighbor_height = grid.(ny).(nx) in
        let current_height = grid.(y).(x) in
        neighbor_height <= current_height + 1
    )
  in

  let rec explore visited queue = 
    match queue with
    | [] -> None  (* Pas de chemin trouvé *)
    | (current_pos, current_dist) :: rest ->
        if current_pos = goal then 
          Some current_dist
        else if PositionSet.mem current_pos visited then
          explore visited rest
        else
          let new_visited = PositionSet.add current_pos visited in
          let neighbors = get_neighbors current_pos in
          (* N'ajoute que les voisins non visités *)
          let new_neighbors = List.filter (fun n -> 
            not (PositionSet.mem n new_visited)
          ) neighbors in
          let new_queue = rest @ List.map (fun n -> (n, current_dist + 1)) new_neighbors in
          explore new_visited new_queue
  in

  explore PositionSet.empty [(start, 0)]
;;

let solve_part1 grid start goal =
  match bfs grid start goal with
  | None -> failwith "No path found"
  | Some dist -> dist
;;

let solve_part2 grid goal =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let starts = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if grid.(y).(x) = 0 then starts := (x, y) :: !starts
    done;
  done;
  List.fold_left (fun min_dist start ->
    match bfs grid start goal with
    | None -> min_dist  (* Pas de chemin depuis ce start *)
    | Some dist -> min min_dist dist
  ) max_int !starts
;;

let () =
  let lines = Utils.read_lines "input/day12.txt" in
  let grid, start, goal = parse_grid lines in
  print_grid grid;
  Printf.printf "Start: (%d, %d)\n" (fst start) (snd start);
  Printf.printf "Goal: (%d, %d)\n" (fst goal) (snd goal);
  let result_part1 = solve_part1 grid start goal in
  let result_part2 = solve_part2 grid goal in
  Printf.printf "Partie 1: %d\n" result_part1;
  Printf.printf "Partie 2: %d\n" result_part2;
