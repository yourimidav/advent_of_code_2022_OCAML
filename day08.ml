(* day 08*)


(*type grid = int array array ;; (* grid.(y).(x) *)

(* Pour stocker la visibilité *)
type visibility_grid = bool array array;; visibilité puis coordonnée*)

let parse_grid lines =
  Array.of_list (
    List.map (fun line ->
      Array.init (String.length line) (fun i ->
        int_of_char line.[i] - int_of_char '0'
      )
    ) lines
  )
;;

let init_visibility grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  Array.make_matrix height width false
;;


let mark_edges_visible vis_grid grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  
  (* Première et dernière ligne *)
  for x = 0 to width - 1 do
    vis_grid.(0).(x) <- true;
    vis_grid.(height - 1).(x) <- true;
  done;
  
  (* Première et dernière colonne *)
  for y = 0 to height - 1 do
    vis_grid.(y).(0) <- true;
    vis_grid.(y).(width - 1) <- true;
  done
;;


(* Depuis la gauche *)
let mark_visible_from_left vis_grid grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  
  for y = 1 to height - 2 do  (* Ignore les bords *)
    let max_height = ref grid.(y).(0) in  (* Max vu depuis la gauche *)
    for x = 1 to width - 2 do
      if grid.(y).(x) > !max_height then begin
        vis_grid.(y).(x) <- true;
        max_height := grid.(y).(x)
      end
    done
  done
;;

(* Depuis la droite *)
let mark_visible_from_right vis_grid grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  
  for y = 1 to height - 2 do
    let max_height = ref grid.(y).(width - 1) in
    for x = width - 2 downto 1 do  (* Parcours de droite à gauche *)
      if grid.(y).(x) > !max_height then begin
        vis_grid.(y).(x) <- true;
        max_height := grid.(y).(x)
      end
    done
  done
;;

(* Depuis le haut *)
let mark_visible_from_top vis_grid grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in

  for x = 1 to width - 2 do  (* Parcourt les colonnes, en ignorant les bords *)
    let max_height = ref grid.(0).(x) in  (* Haut de la colonne *)
    for y = 1 to height - 2 do  (* Parcourt les lignes de haut en bas *)
      if grid.(y).(x) > !max_height then begin
        vis_grid.(y).(x) <- true;
        max_height := grid.(y).(x)
      end
    done
  done
;;

(* Depuis le bas *)
let mark_visible_from_bot vis_grid grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in

  for x = 1 to width - 2 do  (* Parcourt les colonnes, en ignorant les bords *)
    let max_height = ref grid.(height - 1).(x) in  (* Bas de la colonne *)
    for y = height - 2 downto 1 do  (* Parcours de bas en haut *)
      if grid.(y).(x) > !max_height then begin
        vis_grid.(y).(x) <- true;
        max_height := grid.(y).(x)
      end
    done
  done
;;

(* Pareil pour le haut et le bas *)

let count_visible vis_grid =
  Array.fold_left (fun acc row ->
    acc + Array.fold_left (fun acc2 is_vis ->
      if is_vis then acc2 + 1 else acc2
    ) 0 row
  ) 0 vis_grid
;;

(* 6. Main 1 *)
let solve_part1 lines =
  let grid = parse_grid lines in
  let vis_grid = init_visibility grid in
  mark_edges_visible vis_grid grid;
  mark_visible_from_left vis_grid grid;
  mark_visible_from_right vis_grid grid;
  mark_visible_from_top vis_grid grid;
  mark_visible_from_bot vis_grid grid;
  count_visible vis_grid
;;

(* Fonction pour calculer le nombre d'arbres visibles dans une direction *)
let count_visible_trees grid x_start y_start dx dy =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let current_height = grid.(y_start).(x_start) in
  let rec loop x y count =
    if x < 0 || x >= width || y < 0 || y >= height then count
    else if grid.(y).(x) >= current_height then count + 1
    else loop (x + dx) (y + dy) (count + 1)
  in
  loop (x_start + dx) (y_start + dy) 0
;;

(* Calcule le scenic score pour un arbre donné *)
let scenic_score grid x y =
  let up = count_visible_trees grid x y 0 (-1) in
  let down = count_visible_trees grid x y 0 1 in
  let left = count_visible_trees grid x y (-1) 0 in
  let right = count_visible_trees grid x y 1 0 in
  up * down * left * right
;;

(* Résout la partie 2 : trouve le plus grand scenic score *)
let solve_part2 lines =
  let grid = parse_grid lines in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let max_score = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let score = scenic_score grid x y in
      if score > !max_score then max_score := score
    done
  done;
  !max_score
;;

let () =
  let lines = Utils.read_lines "input/day08.txt" in
  let result01 = solve_part1 lines in
  let result02 = solve_part2 lines in
  print_endline("result for part 1 : " ^ string_of_int result01);
  print_endline("\nresult for part 2 : " ^ string_of_int result02);
