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
  | 'E' -> 27
  | _ -> int_of_char letter - int_of_char 'a' + 1

(* Convertit une ligne en un tableau de hauteurs et cherche S/E *)
let parse_line y line =
  let chars = String.to_seq line |> List.of_seq in
  let heights = List.map char_to_height chars |> Array.of_list in
  let find_pos height x c =
    if char_to_height c = height then Some (x, y) else None
  in
  let start_pos = filter_mapi (fun x c -> find_pos 0 x c) chars in
  let goal_pos = filter_mapi (fun x c -> find_pos 27 x c) chars in
  (heights, List.hd start_pos, List.hd goal_pos)

(* Convertit toutes les lignes en une grille et trouve S/E *)
let parse_grid lines =
  let parse_line_with_pos y line =
    let heights, start, goal = parse_line y line in
    (heights, start, goal)
  in
  let lines_with_pos = List.mapi parse_line_with_pos lines in
  let grid = List.map (fun (heights, _, _) -> heights) lines_with_pos |> Array.of_list in
  let starts = List.filter_map (fun (_, start, _) -> start) lines_with_pos in
  let goals = List.filter_map (fun (_, _, goal) -> goal) lines_with_pos in
  let start = match starts with
    | [pos] -> pos
    | _ -> failwith "Start position not found or multiple starts"
  in
  let goal = match goals with
    | [pos] -> pos
    | _ -> failwith "Goal position not found or multiple goals"
  in
  (grid, start, goal)

(* Fonction pour afficher la grille *)
let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun height ->
      let c =
        if height = 0 then 'S'
        else if height = 27 then 'E'
        else char_of_int (height + int_of_char 'a' - 1)
      in
      print_char c
    ) row;
    print_newline ()
  ) grid

let () =
  let lines = Utils.read_lines "input/day12.txt" in
  let grid, start, goal = parse_grid lines in
  print_grid grid;
  Printf.printf "Start: (%d, %d)\n" (fst start) (snd start);
  Printf.printf "Goal: (%d, %d)\n" (fst goal) (snd goal)
