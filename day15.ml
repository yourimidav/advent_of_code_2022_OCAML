(* day 15*)

type position = int * int

type sensor = {
  pos: position;
  beacon: position;
  radius: int;  (* Distance de Manhattan au beacon *)
}

(* Distance de Manhattan *)
let manhattan_distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)
;;

(* Parse une ligne *)
let parse_line line =
  (* Exemple: "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" *)
  
  (* Méthode 1 : Avec Str (regex) *)
  let pattern = Str.regexp 
    "Sensor at x=\\([-0-9]+\\), y=\\([-0-9]+\\): closest beacon is at x=\\([-0-9]+\\), y=\\([-0-9]+\\)" in
  
  if Str.string_match pattern line 0 then
    let sx = int_of_string (Str.matched_group 1 line) in
    let sy = int_of_string (Str.matched_group 2 line) in
    let bx = int_of_string (Str.matched_group 3 line) in
    let by = int_of_string (Str.matched_group 4 line) in
    let sensor_pos = (sx, sy) in
    let beacon_pos = (bx, by) in
    let radius = manhattan_distance sensor_pos beacon_pos in
    { pos = sensor_pos; beacon = beacon_pos; radius = radius }
  else
    failwith ("Failed to parse line: " ^ line)
;;

let parse_sensors lines =
  List.map parse_line lines
;;

(*
let test_parsing () =
  let test_lines = [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
  ] in
  let sensors = parse_sensors test_lines in
  List.iter (fun s ->
    let (sx, sy) = s.pos in
    let (bx, by) = s.beacon in
    Printf.printf "Sensor at (%d, %d), beacon at (%d, %d), radius=%d\n"
      sx sy bx by s.radius
  ) sensors
;; *)

(* Calcule l'intervalle [x_min, x_max] couvert par un capteur sur une ligne y *)
let coverage_on_line sensor target_y =
  let (sx, sy) = sensor.pos in
  let distance_to_line = abs (sy - target_y) in
  
  if distance_to_line > sensor.radius then
    None  (* Le capteur ne couvre pas cette ligne *)
  else
    (* Largeur de couverture à cette hauteur *)
    let width = sensor.radius - distance_to_line in
    Some (sx - width, sx + width)
;;

(* Fusionne des intervalles qui se chevauchent *)
let merge_intervals intervals =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) intervals in
  
  let rec merge acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | (a1, b1) :: (a2, b2) :: rest ->
        if b1 >= a2 - 1 then  (* Chevauchement ou adjacents *)
          merge acc ((a1, max b1 b2) :: rest)
        else
          merge ((a1, b1) :: acc) ((a2, b2) :: rest)
  in
  merge [] sorted
;;

(* Compte le nombre de positions dans des intervalles fusionnés *)
let count_positions intervals =
  List.fold_left (fun acc (a, b) -> acc + (b - a + 1)) 0 intervals
;;


let solve_part1 sensors target_y =
  (* 1. Calculer les intervalles couverts *)
  let intervals = 
    List.filter_map (fun s -> coverage_on_line s target_y) sensors
  in
  
  (* 2. Fusionner les intervalles *)
  let merged = merge_intervals intervals in
  
  (* 3. Compter les positions *)
  let total = count_positions merged in
  
  (* 4. Soustraire les balises qui sont sur cette ligne *)
  let beacons_on_line = 
    sensors
    |> List.map (fun s -> s.beacon)
    |> List.filter (fun (_, by) -> by = target_y)
    |> List.sort_uniq compare
    |> List.length
  in
  
  total - beacons_on_line
;;


let solve_part2 sensors max_coord =
  (* Pour chaque ligne y de 0 à max_coord *)
  let rec check_line y =
    if y > max_coord then
      failwith "No distress beacon found"
    else
      (* Calculer les intervalles couverts sur cette ligne *)
      let intervals = 
        List.filter_map (fun s -> coverage_on_line s y) sensors
      in
      
      (* Fusionner les intervalles *)
      let merged = merge_intervals intervals in
      
      (* Couper les intervalles à [0, max_coord] *)
      let clipped = 
        List.filter_map (fun (a, b) ->
          let a' = max 0 a in
          let b' = min max_coord b in
          if a' <= b' then Some (a', b') else None
        ) merged
      in
      
      (* Chercher un trou *)
      match clipped with
      | [] -> check_line (y + 1)  (* Aucune couverture, impossible normalement *)
      | [(a, b)] ->
          if a = 0 && b = max_coord then
            check_line (y + 1)  (* Ligne complètement couverte *)
          else if a > 0 then
            (* Trou au début *)
            (0, y)
          else
            (* Trou à la fin *)
            (max_coord, y)
      | (_, b1) :: _ ->  (* Changé : on n'a pas besoin de a2, juste b1 *)
          (* Trou entre deux intervalles *)
          (b1 + 1, y)
  in
  
  let (x, y) = check_line 0 in
  (* Tuning frequency = x * 4000000 + y *)
  x * 4000000 + y
;;

let test () =
  let test_lines = [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16";
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16";
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16";
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10";
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10";
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10";
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17";
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22";
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3";
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3";
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3";
  ] in
  let sensors = parse_sensors test_lines in
  
  Printf.printf "=== Test avec l'exemple ===\n";
  let result_part1 = solve_part1 sensors 10 in  (* y=10 pour l'exemple *)
  Printf.printf "Part 1 (y=10): %d (attendu: 26)\n" result_part1;
  
  let result_part2 = solve_part2 sensors 20 in  (* max=20 pour l'exemple *)
  Printf.printf "Part 2: %d (attendu: 56000011)\n" result_part2;
;;

let () =
  test();
  let lines = Utils.read_lines "input/day15.txt" in
  let sensors = parse_sensors lines in
  
  let result_part1 = solve_part1 sensors 2000000 in
  let result_part2 = solve_part2 sensors 4000000 in
  
  print_endline ("day15, part 1 : " ^ string_of_int result_part1);
  print_endline ("day15, part 2 : " ^ string_of_int result_part2);
;;

