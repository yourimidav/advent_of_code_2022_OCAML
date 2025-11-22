(* day 24 *)

type position = int * int
type direction = Up | Down | Left | Right

type blizzard = {
  pos: position;
  dir: direction;
}

(* Parse la carte *)
let parse_map lines =
  let blizzards = ref [] in
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  
  List.iteri (fun y line ->
    String.iteri (fun x c ->
      let pos = (x, y) in
      match c with
      | '^' -> blizzards := { pos; dir = Up } :: !blizzards
      | 'v' -> blizzards := { pos; dir = Down } :: !blizzards
      | '<' -> blizzards := { pos; dir = Left } :: !blizzards
      | '>' -> blizzards := { pos; dir = Right } :: !blizzards
      | _ -> ()
    ) line
  ) lines;
  
  (!blizzards, width, height)
;;

(* Avancer un blizzard *)
let move_blizzard blizzard width height =
  let (x, y) = blizzard.pos in
  let (nx, ny) = match blizzard.dir with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
  in
  
  (* Wrapping (éviter les murs) *)
  let nx = if nx = 0 then width - 2 else if nx = width - 1 then 1 else nx in
  let ny = if ny = 0 then height - 2 else if ny = height - 1 then 1 else ny in
  
  { blizzard with pos = (nx, ny) }
;;

(* Simuler tous les blizzards pour une minute *)
let simulate_blizzards blizzards width height =
  List.map (fun b -> move_blizzard b width height) blizzards
;;

(* Set pour stocker les positions *)
module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)

(* Créer un set des positions occupées par les blizzards *)
let blizzard_positions blizzards =
  List.fold_left (fun set b -> PosSet.add b.pos set) PosSet.empty blizzards
;;

(* BFS pour trouver le chemin le plus court *)
(* BFS pour trouver le chemin le plus court *)
let find_shortest_path start goal blizzards width height =
  (* Précalculer les états des blizzards (ils cyclent après lcm(width-2, height-2)) *)
  let inner_width = width - 2 in
  let inner_height = height - 2 in
  
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  let lcm a b = (a * b) / gcd a b in
  let cycle_length = lcm inner_width inner_height in
  
  let blizzard_states = Array.init cycle_length (fun i ->
    let rec simulate bs n =
      if n = 0 then bs
      else simulate (simulate_blizzards bs width height) (n - 1)
    in
    blizzard_positions (simulate blizzards i)
  ) in
  
  (* BFS *)
  let module StateSet = Set.Make(struct
    type t = int * position  (* time, position *)
    let compare = compare
  end) in
  
  let rec bfs queue visited =
    match queue with
    | [] -> failwith "No path found"
    | (time, pos) :: rest ->
        if pos = goal then time
        else if StateSet.mem (time, pos) visited then
          bfs rest visited
        else
          let new_visited = StateSet.add (time, pos) visited in
          let next_time = time + 1 in
          let next_blizzards = blizzard_states.(next_time mod cycle_length) in
          
          let (x, y) = pos in
          let moves = [
            (x, y);      (* Wait *)
            (x + 1, y);  (* Right *)
            (x - 1, y);  (* Left *)
            (x, y + 1);  (* Down *)
            (x, y - 1);  (* Up *)
          ] in
          
          let valid_moves = List.filter (fun (nx, ny) ->
            (* Vérifier les limites *)
            ((nx = fst start && ny = snd start) ||  (* Position de départ *)
             (nx = fst goal && ny = snd goal) ||    (* Position d'arrivée *)
             (nx > 0 && nx < width - 1 && ny > 0 && ny < height - 1)) &&  (* Dans la vallée *)
            not (PosSet.mem (nx, ny) next_blizzards)  (* Pas de blizzard *)
          ) moves in
          
          let new_queue = rest @ List.map (fun p -> (next_time, p)) valid_moves in
          bfs new_queue new_visited
  in
  
  bfs [(0, start)] StateSet.empty
;;

(* Partie 1 *)
let solve_part1 blizzards width height =
  let start = (1, 0) in
  let goal = (width - 2, height - 1) in
  find_shortest_path start goal blizzards width height
;;

(* Partie 2 *)
let solve_part2 blizzards width height =
  let start = (1, 0) in
  let goal = (width - 2, height - 1) in
  
  Printf.printf "Trip 1: start -> goal...\n%!";
  let time1 = find_shortest_path start goal blizzards width height in
  Printf.printf "  Arrived at time %d\n%!" time1;
  
  (* Simuler les blizzards jusqu'au temps time1 *)
  let rec simulate_n bs n =
    if n = 0 then bs
    else simulate_n (simulate_blizzards bs width height) (n - 1)
  in
  let blizzards_after_trip1 = simulate_n blizzards time1 in
  
  Printf.printf "Trip 2: goal -> start...\n%!";
  let time2 = find_shortest_path goal start blizzards_after_trip1 width height in
  Printf.printf "  Arrived at time %d (total: %d)\n%!" time2 (time1 + time2);
  
  let blizzards_after_trip2 = simulate_n blizzards_after_trip1 time2 in
  
  Printf.printf "Trip 3: start -> goal...\n%!";
  let time3 = find_shortest_path start goal blizzards_after_trip2 width height in
  Printf.printf "  Arrived at time %d (total: %d)\n%!" time3 (time1 + time2 + time3);
  
  time1 + time2 + time3
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day24.txt" in
  let (blizzards, width, height) = parse_map lines in
  
  Printf.printf "Carte: %dx%d, %d blizzards\n\n" width height (List.length blizzards);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 blizzards width height in
  print_endline ("day24, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2...\n";
  let result_part2 = solve_part2 blizzards width height in
  print_endline ("day24, part 2 : " ^ string_of_int result_part2);
;;