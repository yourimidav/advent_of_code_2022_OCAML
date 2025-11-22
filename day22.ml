(* day 22 *)

type cell = Open | Wall | Empty

type direction = Right | Down | Left | Up

type instruction = 
  | Move of int
  | TurnLeft
  | TurnRight

(* Parse la carte *)
let parse_map lines =
  let height = List.length lines in
  let width = List.fold_left (fun m line -> max m (String.length line)) 0 lines in
  
  let grid = Array.make_matrix height width Empty in
  
  List.iteri (fun y line ->
    String.iteri (fun x c ->
      match c with
      | '.' -> grid.(y).(x) <- Open
      | '#' -> grid.(y).(x) <- Wall
      | ' ' -> grid.(y).(x) <- Empty
      | _ -> ()
    ) line
  ) lines;
  
  (grid, height, width)
;;

(* Parse les instructions *)
let parse_instructions line =
  let rec parse acc current pos =
    if pos >= String.length line then
      List.rev (if current <> "" then Move (int_of_string current) :: acc else acc)
    else
      match line.[pos] with
      | 'R' -> 
          let acc = if current <> "" then Move (int_of_string current) :: acc else acc in
          parse (TurnRight :: acc) "" (pos + 1)
      | 'L' ->
          let acc = if current <> "" then Move (int_of_string current) :: acc else acc in
          parse (TurnLeft :: acc) "" (pos + 1)
      | c ->
          parse acc (current ^ String.make 1 c) (pos + 1)
  in
  parse [] "" 0
;;

(* Trouver la position de départ *)
let find_start grid =
  let y = 0 in
  let rec find_x x =
    if grid.(y).(x) = Open then (x, y)
    else find_x (x + 1)
  in
  find_x 0
;;

(* Tourner *)
let turn_left = function
  | Right -> Up | Down -> Right | Left -> Down | Up -> Left
;;

let turn_right = function
  | Right -> Down | Down -> Left | Left -> Up | Up -> Right
;;

(* Direction en delta *)
let direction_delta = function
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Up -> (0, -1)
;;

(* Wrapping partie 1 : trouver la prochaine case non-vide dans la direction *)
let wrap_part1 grid height width (x, y) dir =
  let (dx, dy) = direction_delta dir in
  
  (* Aller dans la direction opposée pour trouver le bord *)
  let rec find_opposite cx cy =
    let nx = (cx - dx + width) mod width in
    let ny = (cy - dy + height) mod height in
    if grid.(ny).(nx) <> Empty then find_opposite nx ny
    else (cx, cy)
  in
  
  find_opposite x y
;;

(* Avancer d'un pas (partie 1) *)
let step_part1 grid height width (x, y) dir =
  let (dx, dy) = direction_delta dir in
  let nx = x + dx in
  let ny = y + dy in
  
  (* Vérifier si on sort de la grille ou dans le vide *)
  if nx < 0 || nx >= width || ny < 0 || ny >= height || grid.(ny).(nx) = Empty then
    wrap_part1 grid height width (x, y) dir
  else
    (nx, ny)
;;

(* Avancer (partie 1) *)
let move_part1 grid height width pos dir steps =
  let rec loop (x, y) remaining =
    if remaining = 0 then (x, y)
    else
      let (nx, ny) = step_part1 grid height width (x, y) dir in
      if grid.(ny).(nx) = Wall then (x, y)  (* Bloqué par un mur *)
      else loop (nx, ny) (remaining - 1)
  in
  loop pos steps
;;

(* Simuler (partie 1) *)
let simulate_part1 grid height width instructions =
  let start_pos = find_start grid in
  let start_dir = Right in
  
  let rec simulate pos dir = function
    | [] -> (pos, dir)
    | Move n :: rest ->
        let new_pos = move_part1 grid height width pos dir n in
        simulate new_pos dir rest
    | TurnLeft :: rest ->
        simulate pos (turn_left dir) rest
    | TurnRight :: rest ->
        simulate pos (turn_right dir) rest
  in
  
  simulate start_pos start_dir instructions
;;

(* Calculer le mot de passe final *)
let calculate_password (x, y) dir =
  let facing = match dir with
    | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3
  in
  1000 * (y + 1) + 4 * (x + 1) + facing
;;

(* Partie 1 *)
let solve_part1 grid height width instructions =
  let (final_pos, final_dir) = simulate_part1 grid height width instructions in
  Printf.printf "Final position: (%d, %d), direction: %s\n"
    (fst final_pos) (snd final_pos)
    (match final_dir with Right -> "R" | Down -> "D" | Left -> "L" | Up -> "U");
  calculate_password final_pos final_dir
;;

(* Partie 2 : Déterminer la structure du cube 
let analyze_cube_structure grid height width =
  (* dans l'input, le cube est déplié comme suit (chaque face = 50x50) :
         [1][2]
         [3]
      [4][5]
      [6]
  *)
  let face_size = 50 in
  face_size
;; *)

(* Wrapping partie 2 : transitions entre faces du cube *)
(* Wrapping partie 2 : transitions entre faces du cube *)
let wrap_part2 _grid (x, y) dir =
  (* Déterminer quelle face on quitte et dans quelle direction *)
  match dir with
  | Right ->
      if y < 50 && x = 149 then
        (* Face 2 → Face 5 (flipé) : (149, y) → (99, 149-y) en Left *)
        ((99, 149 - y), Left)
      else if y >= 50 && y < 100 && x = 99 then
        (* Face 3 → Face 2 : (99, y) → (50+y, 49) en Up *)
        ((50 + y, 49), Up)
      else if y >= 100 && y < 150 && x = 99 then
        (* Face 5 → Face 2 (flipé) : (99, y) → (149, 149-y) en Left *)
        ((149, 149 - y), Left)
      else if y >= 150 && x = 49 then
        (* Face 6 → Face 5 : (49, y) → (y-100, 149) en Up *)
        ((y - 100, 149), Up)
      else failwith (Printf.sprintf "Unhandled right wrap at (%d, %d)" x y)
  
  | Left ->
      if y < 50 && x = 50 then
        (* Face 1 → Face 4 (flipé) : (50, y) → (0, 149-y) en Right *)
        ((0, 149 - y), Right)
      else if y >= 50 && y < 100 && x = 50 then
        (* Face 3 → Face 4 : (50, y) → (y-50, 100) en Down *)
        ((y - 50, 100), Down)
      else if y >= 100 && y < 150 && x = 0 then
        (* Face 4 → Face 1 (flipé) : (0, y) → (50, 149-y) en Right *)
        ((50, 149 - y), Right)
      else if y >= 150 && x = 0 then
        (* Face 6 → Face 1 : (0, y) → (y-100, 0) en Down *)
        ((y - 100, 0), Down)
      else failwith (Printf.sprintf "Unhandled left wrap at (%d, %d)" x y)
  
  | Down ->
      if x < 50 && y = 199 then
        (* Face 6 → Face 2 : (x, 199) → (x+100, 0) en Down *)
        ((x + 100, 0), Down)
      else if x >= 50 && x < 100 && y = 149 then
        (* Face 5 → Face 6 : (x, 149) → (49, x+100) en Left *)
        ((49, x + 100), Left)
      else if x >= 100 && y = 49 then
        (* Face 2 → Face 3 : (x, 49) → (99, x-50) en Left *)
        ((99, x - 50), Left)
      else failwith (Printf.sprintf "Unhandled down wrap at (%d, %d)" x y)
  
  | Up ->
      if x < 50 && y = 100 then
        (* Face 4 → Face 3 : (x, 100) → (50, x+50) en Right *)
        ((50, x + 50), Right)
      else if x >= 50 && x < 100 && y = 0 then
        (* Face 1 → Face 6 : (x, 0) → (0, x+100) en Right *)
        ((0, x + 100), Right)
      else if x >= 100 && y = 0 then
        (* Face 2 → Face 6 : (x, 0) → (x-100, 199) en Up *)
        ((x - 100, 199), Up)
      else failwith (Printf.sprintf "Unhandled up wrap at (%d, %d)" x y)
;;

(* Avancer d'un pas (partie 2) *)
let step_part2 grid (x, y) dir =
  let (dx, dy) = direction_delta dir in
  let nx = x + dx in
  let ny = y + dy in
  
  (* Vérifier si on sort de la grille ou dans le vide *)
  if nx < 0 || nx >= Array.length grid.(0) || ny < 0 || ny >= Array.length grid || grid.(ny).(nx) = Empty then
    wrap_part2 grid (x, y) dir
  else
    ((nx, ny), dir)
;;

(* Avancer (partie 2) *)
let move_part2 grid pos dir steps =
  let rec loop (x, y) current_dir remaining =
    if remaining = 0 then ((x, y), current_dir)
    else
      let ((nx, ny), new_dir) = step_part2 grid (x, y) current_dir in
      if grid.(ny).(nx) = Wall then ((x, y), current_dir)  (* Bloqué par un mur *)
      else loop (nx, ny) new_dir (remaining - 1)
  in
  loop pos dir steps
;;

(* Simuler (partie 2) *)
let simulate_part2 grid instructions =
  let start_pos = find_start grid in
  let start_dir = Right in
  
  let rec simulate pos dir = function
    | [] -> (pos, dir)
    | Move n :: rest ->
        let (new_pos, new_dir) = move_part2 grid pos dir n in
        simulate new_pos new_dir rest
    | TurnLeft :: rest ->
        simulate pos (turn_left dir) rest
    | TurnRight :: rest ->
        simulate pos (turn_right dir) rest
  in
  
  simulate start_pos start_dir instructions
;;

(* Partie 2 mise à jour *)
let solve_part2 grid _height _width instructions =
  let (final_pos, final_dir) = simulate_part2 grid instructions in
  Printf.printf "Final position: (%d, %d), direction: %s\n"
    (fst final_pos) (snd final_pos)
    (match final_dir with Right -> "R" | Down -> "D" | Left -> "L" | Up -> "U");
  calculate_password final_pos final_dir
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day22.txt" in
  
  (* Séparer la carte et les instructions *)
  let rec split_at_empty acc = function
    | [] -> (List.rev acc, [])
    | "" :: rest -> (List.rev acc, rest)
    | line :: rest -> split_at_empty (line :: acc) rest
  in
  
  let (map_lines, instruction_lines) = split_at_empty [] lines in
  let instruction_line = match instruction_lines with
    | [line] -> line
    | line :: _ -> line
    | [] -> failwith "No instructions found"
  in
  
  let (grid, height, width) = parse_map map_lines in
  let instructions = parse_instructions instruction_line in
  
  Printf.printf "Carte: %dx%d\n" width height;
  Printf.printf "Nombre d'instructions: %d\n\n" (List.length instructions);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 grid height width instructions in
  print_endline ("day22, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2...\n";
  let result_part2 = solve_part2 grid height width instructions in
  print_endline ("day22, part 2 : " ^ string_of_int result_part2);
;;