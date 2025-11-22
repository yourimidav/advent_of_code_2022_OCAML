(* day 18 *)

type position = int * int * int

(* Parse une ligne "x,y,z" *)
let parse_line line =
  match String.split_on_char ',' line with
  | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> failwith ("Invalid line: " ^ line)
;;

(* Parse toutes les lignes *)
let parse_cubes lines =
  List.map parse_line lines
;;

(* Set pour stocker les positions *)
module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)

(* Construire le set de cubes *)
let build_cube_set cubes =
  List.fold_left (fun set pos -> PosSet.add pos set) PosSet.empty cubes
;;

(* Les 6 voisins adjacents d'un cube *)
let neighbors (x, y, z) = [
  (x + 1, y, z); (x - 1, y, z);
  (x, y + 1, z); (x, y - 1, z);
  (x, y, z + 1); (x, y, z - 1);
]

(* Partie 1 : Surface totale *)
let solve_part1 cubes =
  let cube_set = build_cube_set cubes in
  
  (* Pour chaque cube, compter combien de faces sont exposées *)
  List.fold_left (fun total_surface pos ->
    let exposed_faces = List.fold_left (fun count neighbor ->
      if PosSet.mem neighbor cube_set then count
      else count + 1
    ) 0 (neighbors pos) in
    total_surface + exposed_faces
  ) 0 cubes
;;

(* Partie 2 : Surface extérieure seulement *)
let solve_part2 cubes =
  let cube_set = build_cube_set cubes in
  
  (* Trouver les limites de la grille *)
  let min_x = List.fold_left (fun m (x, _, _) -> min m x) max_int cubes - 1 in
  let max_x = List.fold_left (fun m (x, _, _) -> max m x) min_int cubes + 1 in
  let min_y = List.fold_left (fun m (_, y, _) -> min m y) max_int cubes - 1 in
  let max_y = List.fold_left (fun m (_, y, _) -> max m y) min_int cubes + 1 in
  let min_z = List.fold_left (fun m (_, _, z) -> min m z) max_int cubes - 1 in
  let max_z = List.fold_left (fun m (_, _, z) -> max m z) min_int cubes + 1 in
  
  (* Vérifier si une position est dans les limites *)
  let in_bounds (x, y, z) =
    x >= min_x && x <= max_x &&
    y >= min_y && y <= max_y &&
    z >= min_z && z <= max_z
  in
  
  (* BFS pour trouver tous les cubes d'air extérieurs *)
  let rec bfs queue visited =
    match queue with
    | [] -> visited
    | pos :: rest ->
        if PosSet.mem pos visited || PosSet.mem pos cube_set then
          bfs rest visited
        else if not (in_bounds pos) then
          bfs rest visited
        else
          let new_visited = PosSet.add pos visited in
          let new_queue = rest @ neighbors pos in
          bfs new_queue new_visited
  in
  
  (* Commencer depuis un coin extérieur *)
  let start = (min_x, min_y, min_z) in
  let exterior_air = bfs [start] PosSet.empty in
  
  (* Compter les faces exposées à l'air extérieur *)
  List.fold_left (fun total_surface pos ->
    let exposed_faces = List.fold_left (fun count neighbor ->
      if PosSet.mem neighbor exterior_air then count + 1
      else count
    ) 0 (neighbors pos) in
    total_surface + exposed_faces
  ) 0 cubes
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day18.txt" in
  let cubes = parse_cubes lines in
  
  Printf.printf "Nombre de cubes: %d\n\n" (List.length cubes);
  
  let result_part1 = solve_part1 cubes in
  print_endline ("day18, part 1 : " ^ string_of_int result_part1);
  
  let result_part2 = solve_part2 cubes in
  print_endline ("day18, part 2 : " ^ string_of_int result_part2);
;;