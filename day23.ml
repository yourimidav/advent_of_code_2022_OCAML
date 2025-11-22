(* day 23 *)

type position = int * int

(* Parse la carte *)
let parse_elves lines =
  let elves = ref [] in
  List.iteri (fun y line ->
    String.iteri (fun x c ->
      if c = '#' then
        elves := (x, y) :: !elves
    ) line
  ) lines;
  !elves
;;

(* Set pour stocker les positions *)
module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)

(* Les 8 directions autour d'une position *)
let neighbors (x, y) = [
  (x-1, y-1); (x, y-1); (x+1, y-1);  (* N *)
  (x-1, y);             (x+1, y);    (* W, E *)
  (x-1, y+1); (x, y+1); (x+1, y+1);  (* S *)
]

(* Vérifier si un elfe a des voisins *)
let has_neighbors pos elves_set =
  List.exists (fun n -> PosSet.mem n elves_set) (neighbors pos)
;;

(* Les 4 directions à considérer (ordre rotatif) *)
type check_dir = North | South | West | East

let get_checks dir (x, y) =
  match dir with
  | North -> ([(x-1, y-1); (x, y-1); (x+1, y-1)], (x, y-1))  (* NW, N, NE → move N *)
  | South -> ([(x-1, y+1); (x, y+1); (x+1, y+1)], (x, y+1))  (* SW, S, SE → move S *)
  | West ->  ([(x-1, y-1); (x-1, y); (x-1, y+1)], (x-1, y))  (* NW, W, SW → move W *)
  | East ->  ([(x+1, y-1); (x+1, y); (x+1, y+1)], (x+1, y))  (* NE, E, SE → move E *)
;;

(* Proposer un mouvement pour un elfe *)
let propose_move pos elves_set directions =
  if not (has_neighbors pos elves_set) then
    None  (* Pas de voisins, ne bouge pas *)
  else
    (* Essayer chaque direction dans l'ordre *)
    let rec try_dirs = function
      | [] -> None  (* Aucune direction valide *)
      | dir :: rest ->
          let (checks, target) = get_checks dir pos in
          if List.for_all (fun p -> not (PosSet.mem p elves_set)) checks then
            Some target  (* Cette direction est libre *)
          else
            try_dirs rest
    in
    try_dirs directions
;;

(* Simuler un round *)
let simulate_round elves directions =
  let elves_set = List.fold_left (fun set pos -> PosSet.add pos set) PosSet.empty elves in
  
  (* Phase 1 : Chaque elfe propose un mouvement *)
  let proposals = List.map (fun pos ->
    (pos, propose_move pos elves_set directions)
  ) elves in
  
  (* Phase 2 : Compter combien d'elfes veulent aller à chaque position *)
  let target_counts = Hashtbl.create 1000 in
  List.iter (fun (_pos, proposal) ->
    match proposal with
    | None -> ()
    | Some target ->
        let count = try Hashtbl.find target_counts target with Not_found -> 0 in
        Hashtbl.replace target_counts target (count + 1)
  ) proposals;
  
  (* Phase 3 : Déplacer les elfes (uniquement si seuls à vouloir aller là) *)
  let new_elves = List.map (fun (pos, proposal) ->
    match proposal with
    | None -> pos
    | Some target ->
        let count = Hashtbl.find target_counts target in
        if count = 1 then target else pos
  ) proposals in
  
  (* Vérifier si des elfes ont bougé *)
  let moved = List.exists2 (fun old_pos new_pos -> old_pos <> new_pos) elves new_elves in
  
  (new_elves, moved)
;;

(* Rotation des directions *)
let rotate_directions = function
  | [] -> []
  | hd :: tl -> tl @ [hd]  (* Déplacer le premier à la fin *)
;;

(* Calculer le rectangle englobant *)
let bounding_box elves =
  let min_x = List.fold_left (fun m (x, _) -> min m x) max_int elves in
  let max_x = List.fold_left (fun m (x, _) -> max m x) min_int elves in
  let min_y = List.fold_left (fun m (_, y) -> min m y) max_int elves in
  let max_y = List.fold_left (fun m (_, y) -> max m y) min_int elves in
  (min_x, max_x, min_y, max_y)
;;

(* Compter les cases vides *)
let count_empty_tiles elves =
  let (min_x, max_x, min_y, max_y) = bounding_box elves in
  let width = max_x - min_x + 1 in
  let height = max_y - min_y + 1 in
  let area = width * height in
  area - List.length elves
;;

(* Partie 1 : 10 rounds *)
let solve_part1 elves =
  let rec simulate elves directions round =
    if round > 10 then
      count_empty_tiles elves
    else begin
      let (new_elves, _moved) = simulate_round elves directions in
      let new_directions = rotate_directions directions in
      simulate new_elves new_directions (round + 1)
    end
  in
  simulate elves [North; South; West; East] 1
;;

(* Partie 2 avec meilleurs logs *)
let solve_part2 elves =
  let rec simulate elves directions round =
    if round mod 100 = 0 then begin
      Printf.printf "Round %d...\n%!" round;
      let (min_x, max_x, min_y, max_y) = bounding_box elves in
      Printf.printf "  Bounding box: (%d,%d) to (%d,%d)\n%!" min_x min_y max_x max_y;
    end;
    
    let (new_elves, moved) = simulate_round elves directions in
    
    if round <= 5 then begin
      (* Compter combien ont vraiment bougé *)
      let num_moved = List.fold_left2 (fun count old_pos new_pos ->
        if old_pos <> new_pos then count + 1 else count
      ) 0 elves new_elves in
      Printf.printf "Round %d: %d elfes ont bougé (moved=%b)\n" round num_moved moved;
      Printf.printf "  Directions: %s\n" 
        (String.concat ", " (List.map (function 
          | North -> "N" | South -> "S" | West -> "W" | East -> "E") directions));
    end;
    
    if not moved then begin
      Printf.printf "Aucun mouvement au round %d!\n" round;
      round
    end
    else begin
      let new_directions = rotate_directions directions in
      simulate new_elves new_directions (round + 1)
    end
  in
  simulate elves [North; South; West; East] 1
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day23.txt" in
  let elves = parse_elves lines in
  
  Printf.printf "Nombre d'elfes: %d\n\n" (List.length elves);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 elves in
  print_endline ("day23, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2...\n";
  let result_part2 = solve_part2 elves in
  print_endline ("day23, part 2 : " ^ string_of_int result_part2);
;;