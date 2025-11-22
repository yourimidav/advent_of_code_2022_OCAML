(* day 16 *)
type valve = {
  name: string;
  flow_rate: int;
  tunnels: string list;
}

(* Parse une ligne *)
let parse_line line =
  let valve_pattern = Str.regexp
    "Valve \\([A-Z]+\\) has flow rate=\\([0-9]+\\); tunnels? leads? to valves? \\(.*\\)" in

  if Str.string_match valve_pattern line 0 then
    let name = Str.matched_group 1 line in
    let flow_rate = int_of_string (Str.matched_group 2 line) in
    let tunnels_str = Str.matched_group 3 line in
    let tunnels =
      String.split_on_char ',' tunnels_str
      |> List.map String.trim
    in
    { name; flow_rate; tunnels }
  else
    failwith ("Failed to parse line: " ^ line)

(* Parse toutes les lignes *)
let parse_valves lines =
  List.map parse_line lines

(* Map pour accéder rapidement aux valves par nom *)
module StringMap = Map.Make(String)
type graph = valve StringMap.t
let build_graph valves : graph =
  List.fold_left (fun map v ->
    StringMap.add v.name v map
  ) StringMap.empty valves

(* Calculer les distances avec Floyd-Warshall *)
let compute_distances graph =
  let valve_names = StringMap.fold (fun k _ acc -> k :: acc) graph [] in
  let n = List.length valve_names in

  (* Créer une map nom -> index *)
  let name_to_idx =
    List.mapi (fun i name -> (name, i)) valve_names
    |> List.to_seq
    |> StringMap.of_seq
  in

  let idx_to_name = Array.of_list valve_names in

  (* Initialiser la matrice de distances *)
  let dist = Array.make_matrix n n 1000000 in

  (* Distance de chaque nœud à lui-même = 0 *)
  for i = 0 to n - 1 do
    dist.(i).(i) <- 0
  done;

  (* Distance = 1 pour les tunnels directs *)
  StringMap.iter (fun name valve ->
    let i = StringMap.find name name_to_idx in
    List.iter (fun neighbor ->
      let j = StringMap.find neighbor name_to_idx in
      dist.(i).(j) <- 1
    ) valve.tunnels
  ) graph;

  (* Floyd-Warshall *)
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if dist.(i).(k) + dist.(k).(j) < dist.(i).(j) then
          dist.(i).(j) <- dist.(i).(k) + dist.(k).(j)
      done
    done
  done;

  (dist, name_to_idx, idx_to_name)

(* Fonction helper pour obtenir la distance entre deux valves *)
let get_distance dist name_to_idx v1 v2 =
  let i = StringMap.find v1 name_to_idx in
  let j = StringMap.find v2 name_to_idx in
  dist.(i).(j)

let get_useful_valves graph =
  StringMap.fold (fun name valve acc ->
    if valve.flow_rate > 0 then name :: acc
    else acc
  ) graph []

(* État : position actuelle, temps restant, set de valves ouvertes *)
module StateSet = Set.Make(String)
(* Clé pour la mémoïsation : (position, temps, valves_ouvertes_triées) *)
type memo_key = string * int * string list
module MemoMap = Map.Make(struct
  type t = memo_key
  let compare = compare
end)

(* Recherche avec mémoïsation pour la partie 1 et la partie 2 *)
let solve_part1 graph dist name_to_idx =
  let useful_valves = get_useful_valves graph in

  (* Mémoïsation *)
  let memo = ref MemoMap.empty in

  (* Fonction récursive *)
  let rec search current_pos time_left opened =
    (* Clé pour la mémoïsation *)
    let opened_list = StateSet.elements opened |> List.sort String.compare in
    let key = (current_pos, time_left, opened_list) in

    (* Vérifier si déjà calculé *)
    match MemoMap.find_opt key !memo with
    | Some result -> result
    | None ->
        (* Si plus de temps, on arrête *)
        if time_left <= 0 then 0
        else
          (* Essayer d'ouvrir chaque valve non ouverte *)
          let best = List.fold_left (fun best_so_far valve_name ->
            (* Si déjà ouverte, on ignore *)
            if StateSet.mem valve_name opened then best_so_far
            else
              (* Distance jusqu'à cette valve *)
              let dist_to_valve = get_distance dist name_to_idx current_pos valve_name in
              (* Temps pour y aller + ouvrir (1 minute) *)
              let time_needed = dist_to_valve + 1 in

              if time_needed >= time_left then best_so_far
              else
                (* Temps restant après ouverture *)
                let new_time = time_left - time_needed in
                (* Pression relâchée par cette valve *)
                let valve = StringMap.find valve_name graph in
                let pressure_released = valve.flow_rate * new_time in

                (* Continuer la recherche *)
                let new_opened = StateSet.add valve_name opened in
                let future_pressure = search valve_name new_time new_opened in
                let total_pressure = pressure_released + future_pressure in

                max best_so_far total_pressure
          ) 0 useful_valves in

          (* Mémoriser le résultat *)
          memo := MemoMap.add key best !memo;
          best
  in

  (* Commencer à AA avec 30 minutes *)
  search "AA" 30 StateSet.empty

(* Recherche avec mémoïsation pour la partie 2 *)
and search_with_allowed graph dist name_to_idx current_pos time_left opened allowed =
  let memo = ref MemoMap.empty in

  let rec search current_pos time_left opened =
    let opened_list = StateSet.elements opened |> List.sort String.compare in
    let key = (current_pos, time_left, opened_list) in

    match MemoMap.find_opt key !memo with
    | Some result -> result
    | None ->
        if time_left <= 0 then 0
        else
          (* CORRECTION : fold prend (fonction, set, accumulateur) *)
          let best = StateSet.fold (fun valve_name best_so_far ->
            if StateSet.mem valve_name opened then best_so_far
            else
              let dist_to_valve = get_distance dist name_to_idx current_pos valve_name in
              let time_needed = dist_to_valve + 1 in

              if time_needed >= time_left then best_so_far
              else
                let new_time = time_left - time_needed in
                let valve = StringMap.find valve_name graph in
                let pressure_released = valve.flow_rate * new_time in

                let new_opened = StateSet.add valve_name opened in
                let future_pressure = search valve_name new_time new_opened in
                let total_pressure = pressure_released + future_pressure in

                max best_so_far total_pressure
          ) allowed 0 in  (* CORRECTION : allowed puis 0, pas 0 puis allowed *)

          memo := MemoMap.add key best !memo;
          best
  in

  search current_pos time_left opened

(* Partie 2 : avec l'éléphant (26 minutes chacun) *)
let solve_part2 graph dist name_to_idx =
  let useful_valves = get_useful_valves graph in
  let n = List.length useful_valves in

  (* Pour chaque partition possible des valves *)
  let best = ref 0 in

  (* Générer toutes les partitions avec bitmask *)
  for mask = 0 to (1 lsl n) - 1 do
    (* Partition 1 : valves où le bit est 1 *)
    let my_valves = ref StateSet.empty in
    let elephant_valves = ref StateSet.empty in

    List.iteri (fun i valve ->
      if (mask land (1 lsl i)) <> 0 then
        my_valves := StateSet.add valve !my_valves
      else
        elephant_valves := StateSet.add valve !elephant_valves
    ) useful_valves;

    (* Calculer le score pour chaque partition *)
    let my_score = search_with_allowed graph dist name_to_idx "AA" 26 StateSet.empty !my_valves in
    let elephant_score = search_with_allowed graph dist name_to_idx "AA" 26 StateSet.empty !elephant_valves in

    best := max !best (my_score + elephant_score);

    if mask mod 1000 = 0 then
      Printf.printf "Progress: %d / %d\n%!" mask ((1 lsl n) - 1);
  done;

  !best

let () =
  (* Test du parsing *)
  (* test_parsing (); *)

  Printf.printf "\n\n=== Avec le vrai input ===\n";
  let lines = Utils.read_lines "input/day16.txt" in
  let valves = parse_valves lines in
  let graph = build_graph valves in
  let (dist, name_to_idx, _) = compute_distances graph in

  Printf.printf "Partie 1...\n%!";
  let result_part1 = solve_part1 graph dist name_to_idx in
  print_endline ("day16, part 1 : " ^ string_of_int result_part1);

  Printf.printf "\nPartie 2 (peut prendre du temps)...\n%!";
  let result_part2 = solve_part2 graph dist name_to_idx in
  print_endline ("day16, part 2 : " ^ string_of_int result_part2);
