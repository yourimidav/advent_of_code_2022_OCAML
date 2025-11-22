(* day 17 *)

(* Direction du jet *)
type direction = Left | Right

(* Parse l'input (une seule ligne de < et >) *)
let parse_jets line =
  String.to_seq line
  |> Seq.map (function
      | '<' -> Left
      | '>' -> Right
      | _ -> failwith "Invalid jet character"
    )
  |> List.of_seq
;;

(* Test du parsing 
let test_parsing () =
  let test_input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" in
  let jets = parse_jets test_input in
  
  Printf.printf "Nombre de jets: %d\n" (List.length jets);
  Printf.printf "Premiers jets: ";
  List.iteri (fun i dir ->
    if i < 10 then
      Printf.printf "%s " (match dir with Left -> "<" | Right -> ">")
  ) jets;
  Printf.printf "\n";
;;*)

(* Position relative (x, y) dans une forme *)
type position = int * int

(* Les 5 formes de roches (positions relatives) *)
let shapes = [|
  (* Forme 0: #### *)
  [(0, 0); (1, 0); (2, 0); (3, 0)];
  
  (* Forme 1: + *)
  [(1, 0); (0, 1); (1, 1); (2, 1); (1, 2)];
  
  (* Forme 2: ⌐ *)
  [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)];
  
  (* Forme 3: | *)
  [(0, 0); (0, 1); (0, 2); (0, 3)];
  
  (* Forme 4: ■ *)
  [(0, 0); (1, 0); (0, 1); (1, 1)];
|]
;;


(* Set pour stocker les positions occupées *)
module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)

type chamber = {
  occupied: PosSet.t;  (* Positions occupées par des roches *)
  height: int;         (* Hauteur maximale actuelle *)
}

(* Créer une chambre vide *)
let empty_chamber () = {
  occupied = PosSet.empty;
  height = 0;
}
;;

(* Largeur de la chambre *)
let chamber_width = 7 ;;

(* Calculer les positions absolues d'une forme à une position donnée *)
let get_rock_positions shape (x, y) =
  List.map (fun (dx, dy) -> (x + dx, y + dy)) shape
;;

(* Vérifier si une position est valide *)
let is_valid_position chamber shape (x, y) =
  let positions = get_rock_positions shape (x, y) in
  List.for_all (fun (px, py) ->
    (* Vérifier les limites *)
    px >= 0 && px < chamber_width && py >= 0 &&
    (* Vérifier les collisions *)
    not (PosSet.mem (px, py) chamber.occupied)
  ) positions
;;

(* Appliquer un jet (déplacement horizontal) *)
let apply_jet chamber shape pos direction =
  let (x, y) = pos in
  let new_x = match direction with
    | Left -> x - 1
    | Right -> x + 1
  in
  let new_pos = (new_x, y) in
  if is_valid_position chamber shape new_pos then
    new_pos
  else
    pos  (* Reste à la même position si invalide *)
;;

(* Faire tomber la roche d'une unité *)
let fall chamber shape pos =
  let (x, y) = pos in
  let new_pos = (x, y - 1) in
  if is_valid_position chamber shape new_pos then
    Some new_pos  (* Peut tomber *)
  else
    None  (* Se pose *)
;;

(* Ajouter une roche à la chambre *)
let add_rock chamber shape pos =
  let positions = get_rock_positions shape pos in
  let new_occupied = List.fold_left (fun acc p ->
    PosSet.add p acc
  ) chamber.occupied positions in
  
  (* Calculer la nouvelle hauteur *)
  let max_y = List.fold_left (fun max_y (_, y) ->
    max max_y (y + 1)
  ) chamber.height positions in
  
  { occupied = new_occupied; height = max_y }
;;

(* Position de départ : 2 unités du bord gauche, 3 unités au-dessus du plus haut *)
let start_position chamber =
  (2, chamber.height + 3)
;;

(*
(* Afficher la chambre (pour debug) *)
let print_chamber chamber max_height =
  for y = max_height downto 0 do
    Printf.printf "|";
    for x = 0 to chamber_width - 1 do
      if PosSet.mem (x, y) chamber.occupied then
        Printf.printf "#"
      else
        Printf.printf "."
    done;
    Printf.printf "|\n";
  done;
  Printf.printf "+";
  for _ = 0 to chamber_width - 1 do
    Printf.printf "-"
  done;
  Printf.printf "+\n";
;;


let test_logic () =
  Printf.printf "\n=== Test de la logique ===\n";
  
  let chamber = empty_chamber () in
  let shape = shapes.(0) in  (* Forme horizontale #### *)
  let pos = start_position chamber in
  
  Printf.printf "Position de départ: (%d, %d)\n" (fst pos) (snd pos);
  Printf.printf "Forme: ";
  List.iter (fun (x, y) -> Printf.printf "(%d,%d) " x y) shape;
  Printf.printf "\n";
  
  (* Test déplacement *)
  let pos_after_left = apply_jet chamber shape pos Left in
  Printf.printf "Après jet gauche: (%d, %d)\n" (fst pos_after_left) (snd pos_after_left);
  
  let pos_after_right = apply_jet chamber shape pos Right in
  Printf.printf "Après jet droite: (%d, %d)\n" (fst pos_after_right) (snd pos_after_right);
  
  (* Test chute *)
  Printf.printf "Peut tomber: %b\n" (fall chamber shape pos <> None);
  
  (* Test ajout de roche *)
  Printf.printf "\nTest ajout de roche au sol:\n";
  let floor_pos = (2, 0) in
  let chamber_with_rock = add_rock chamber shape floor_pos in
  Printf.printf "Hauteur après ajout: %d\n" chamber_with_rock.height;
  Printf.printf "Positions occupées: %d\n" (PosSet.cardinal chamber_with_rock.occupied);
  
  (* Afficher la chambre *)
  Printf.printf "\nChambre après ajout:\n";
  print_chamber chamber_with_rock 5;
  
  (* Test avec une deuxième roche *)
  Printf.printf "\nAjout d'une deuxième roche:\n";
  let shape2 = shapes.(1) in  (* Forme + *)
  let pos2 = start_position chamber_with_rock in
  Printf.printf "Position de départ roche 2: (%d, %d)\n" (fst pos2) (snd pos2);
  let final_chamber = add_rock chamber_with_rock shape2 (2, 1) in
  Printf.printf "Hauteur finale: %d\n" final_chamber.height;
  print_chamber final_chamber 8;
;;

*)

(* Simuler la chute d'une roche *)
let simulate_rock chamber shape jets jet_index =
  let rec loop pos jet_idx =
    (* 1. Appliquer le jet *)
    let jet_direction = List.nth jets (jet_idx mod List.length jets) in
    let pos_after_jet = apply_jet chamber shape pos jet_direction in
    
    (* 2. Essayer de tomber *)
    match fall chamber shape pos_after_jet with
    | Some new_pos -> 
        (* Continue de tomber *)
        loop new_pos (jet_idx + 1)
    | None -> 
        (* Se pose *)
        (pos_after_jet, jet_idx + 1)
  in
  
  let start_pos = start_position chamber in
  loop start_pos jet_index
;;

(* Partie 1 : simuler n roches *)
let solve_part1 jets n =
  let rec simulate chamber rock_index jet_index =
    if rock_index >= n then
      chamber.height
    else
      let shape = shapes.(rock_index mod Array.length shapes) in
      let (final_pos, new_jet_index) = simulate_rock chamber shape jets jet_index in
      let new_chamber = add_rock chamber shape final_pos in
      
      (* Afficher la progression *)
      if rock_index mod 100 = 0 then
        Printf.printf "Rock %d/%d, height=%d\n%!" rock_index n new_chamber.height;
      
      simulate new_chamber (rock_index + 1) new_jet_index
  in
  
  simulate (empty_chamber ()) 0 0
;;

(* Détection de cycle pour la partie 2 *)
module StateMap = Map.Make(struct
  type t = int * int  (* (rock_index mod 5, jet_index mod len_jets) *)
  let compare = compare
end)

(* Fonction pour obtenir les N dernières lignes (pour détecter les patterns) *)
let get_top_rows chamber n_rows =
  if chamber.height = 0 then []
  else
    let rows = ref [] in
    for y = chamber.height - 1 downto max 0 (chamber.height - n_rows) do
      let row = ref 0 in
      for x = 0 to chamber_width - 1 do
        if PosSet.mem (x, y) chamber.occupied then
          row := !row lor (1 lsl x)
      done;
      rows := !row :: !rows
    done;
    !rows
;;

(* Partie 2 avec détection de cycle *)
let solve_part2 jets target =
  let rec simulate chamber rock_index jet_index seen =
    if rock_index >= target then
      chamber.height
    else
      let shape_idx = rock_index mod Array.length shapes in
      let jet_idx_mod = jet_index mod List.length jets in
      
      (* État pour détecter les cycles *)
      let state_key = (shape_idx, jet_idx_mod) in
      let top_pattern = get_top_rows chamber 30 in
      
      (* Vérifier si on a déjà vu cet état *)
      (match StateMap.find_opt state_key seen with
       | Some (prev_rock_idx, prev_height, prev_pattern) when prev_pattern = top_pattern ->
           (* Cycle détecté ! *)
           let cycle_length = rock_index - prev_rock_idx in
           let height_gain = chamber.height - prev_height in
           
           Printf.printf "Cycle détecté!\n";
           Printf.printf "  Position: rock %d\n" rock_index;
           Printf.printf "  Longueur du cycle: %d roches\n" cycle_length;
           Printf.printf "  Gain de hauteur: %d\n" height_gain;
           
           (* Combien de cycles complets peut-on faire ? *)
           let remaining_rocks = target - rock_index in
           let full_cycles = remaining_rocks / cycle_length in
           let leftover_rocks = remaining_rocks mod cycle_length in
           
           Printf.printf "  Cycles complets: %d\n" full_cycles;
           Printf.printf "  Roches restantes: %d\n" leftover_rocks;
           
           (* Calculer la hauteur finale *)
           let height_from_cycles = full_cycles * height_gain in
           
           (* Simuler les roches restantes *)
           let final_height = simulate_remaining 
             chamber 
             rock_index 
             jet_index 
             leftover_rocks 
           in
           
           chamber.height + height_from_cycles + (final_height - chamber.height)
       | _ ->
           (* Pas de cycle, continuer *)
           let shape = shapes.(shape_idx) in
           let (final_pos, new_jet_index) = simulate_rock chamber shape jets jet_index in
           let new_chamber = add_rock chamber shape final_pos in
           
           let new_seen = StateMap.add state_key 
             (rock_index, chamber.height, top_pattern) seen in
           
           if rock_index mod 1000 = 0 then
             Printf.printf "Rock %d, height=%d\n%!" rock_index new_chamber.height;
           
           simulate new_chamber (rock_index + 1) new_jet_index new_seen
      )
  
  and simulate_remaining chamber rock_index jet_index remaining =
    if remaining <= 0 then
      chamber.height
    else
      let shape = shapes.(rock_index mod Array.length shapes) in
      let (final_pos, new_jet_index) = simulate_rock chamber shape jets jet_index in
      let new_chamber = add_rock chamber shape final_pos in
      simulate_remaining new_chamber (rock_index + 1) new_jet_index (remaining - 1)
  in
  
  simulate (empty_chamber ()) 0 0 StateMap.empty
;;

let () =
  (* Tests *)
  (* Printf.printf "=== Day 17 - Parsing ===\n";
  test_parsing ();
  
  Printf.printf "\n=== Day 17 - Logic ===\n";
  test_logic (); *)
  
  (* Solution *)
  Printf.printf "\n=== Day 17 - Solution ===\n";
  let lines = Utils.read_lines "input/day17.txt" in
  let jets = parse_jets (List.hd lines) in
  
  Printf.printf "Nombre de jets: %d\n\n" (List.length jets);
  
  Printf.printf "Partie 1 (2022 roches)...\n";
  let result_part1 = solve_part1 jets 2022 in
  print_endline ("day17, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2 (1000000000000 roches)...\n";
  let result_part2 = solve_part2 jets 1000000000000 in
  print_endline ("day17, part 2 : " ^ string_of_int result_part2);
;;