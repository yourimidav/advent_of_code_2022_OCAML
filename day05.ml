(* day 05 - Corrigé *)

(* Extrait les caisses d'une ligne du format "[X] [Y] [Z]" *)
let extract_crates_from_line line =
  let length = String.length line in
  let num_slots = (length + 1) / 4 in
  let rec loop pos acc =
    if pos >= num_slots then
      List.rev acc
    else
      let char_pos = pos * 4 + 1 in
      if char_pos < length then
        let c = line.[char_pos] in
        loop (pos + 1) (c :: acc)
      else
        loop (pos + 1) (' ' :: acc)
  in
  loop 0 []
;;

(* Lit les lignes représentant les piles jusqu'à la ligne vide *)
let read_stack_lines lines =
  Printf.printf "[LOG] Début de read_stack_lines, nombre de lignes : %d\n" (List.length lines);
  let rec loop acc = function
    | [] ->
        Printf.printf "[LOG] Fin de read_stack_lines, nombre de lignes de piles extraites : %d\n" (List.length acc);
        List.rev acc
    | line :: tl ->
        Printf.printf "[LOG] Traitement de la ligne : '%s'\n" line;
        (* Ignorer la ligne de numérotation (qui contient " 1   2   3 ") *)
        if String.contains line '1' then begin
          Printf.printf "[LOG] Ligne de numérotation détectée, on l'ignore\n";
          loop acc tl
        end else if String.trim line = "" then begin
          Printf.printf "[LOG] Ligne vide détectée, fin des lignes de piles\n";
          List.rev acc
        end else begin
          let crates = extract_crates_from_line line in
          Printf.printf "[LOG] Caisses extraites : %s\n"
            (String.concat ", " (List.map (fun c -> "'" ^ String.make 1 c ^ "'") crates));
          loop (crates :: acc) tl
        end
  in
  loop [] lines
;;

(* Construit les piles à partir des lignes lues (en inversant pour avoir le bon ordre) *)
let build_stacks stack_lines =
  match stack_lines with
  | [] -> [||]
  | first_line :: _ ->
      let num_stacks = List.length first_line in
      let stacks = Array.make num_stacks [] in
      (* Remplir les piles en inversant l'ordre (du bas vers le haut) *)
      List.iter (fun crates ->
        List.iteri (fun i c ->
          if c <> ' ' then
            stacks.(i) <- c :: stacks.(i)
        ) crates
      ) (List.rev stack_lines);  (* IMPORTANT : inverser pour avoir le bon ordre *)
      stacks
;;

(* Parse toutes les lignes de piles *)
let parse_stacks lines =
  let stack_lines = read_stack_lines lines in
  build_stacks stack_lines
;;

(* Test unitaire des caisses *)
let test_parse_stacks () =
  let test_lines = [
    "    [D]    ";
    "[N] [C]    ";
    "[Z] [M] [P]";
    " 1   2   3 ";
    "";
  ] in
  let stacks = parse_stacks test_lines in
  (* Vérifie que les piles sont bien construites *)
  let pile_0 = stacks.(0) in
  let pile_1 = stacks.(1) in
  let pile_2 = stacks.(2) in
  
  Printf.printf "Pile 0: %s\n" (String.concat ", " (List.map (String.make 1) pile_0));
  Printf.printf "Pile 1: %s\n" (String.concat ", " (List.map (String.make 1) pile_1));
  Printf.printf "Pile 2: %s\n" (String.concat ", " (List.map (String.make 1) pile_2));
  
  (* Retourne une chaîne pour afficher le résultat du test *)
  if pile_0 = ['Z'; 'N'] && pile_1 = ['M'; 'C'; 'D'] && pile_2 = ['P'] then
    "Test parse_stacks : OK"
  else
    "Test parse_stacks : ÉCHEC"
;;

(* Parse une instruction du format "move X from Y to Z" *)
let parse_instruction line =
  let parts = String.split_on_char ' ' line in
  match parts with
  | ["move"; x; "from"; y; "to"; z] ->
      (int_of_string x, int_of_string y - 1, int_of_string z - 1)
  | _ -> failwith ("Format d'instruction incorrect: " ^ line)
;;

(* Effectue un mouvement de caisses (une par une pour partie 1) *)
let move stacks (x, from_idx, to_idx) =
  let rec move_n n =
    if n <= 0 then ()
    else
      match stacks.(from_idx) with
      | [] -> ()
      | crate :: rest ->
          stacks.(from_idx) <- rest;
          stacks.(to_idx) <- crate :: stacks.(to_idx);
          move_n (n - 1)
  in
  move_n x
;;

(* Effectue un mouvement de caisses (toutes en même temps pour partie 2) *)
let move_part2 stacks (x, from_idx, to_idx) =
  (* Prend x caisses du sommet de from_idx *)
  let rec take n lst acc =
    if n <= 0 then (List.rev acc, lst)
    else
      match lst with
      | [] -> (List.rev acc, [])
      | h :: t -> take (n - 1) t (h :: acc)
  in
  let (crates_to_move, remaining) = take x stacks.(from_idx) [] in
  stacks.(from_idx) <- remaining;
  stacks.(to_idx) <- crates_to_move @ stacks.(to_idx)
;;

(* Sépare les lignes de piles et les instructions *)
let split_stacks_and_instructions lines =
  let rec loop_stack acc = function
    | [] -> (List.rev acc, [])
    | line :: tl ->
        if String.trim line = "" then
          (List.rev acc, tl)
        else
          loop_stack (line :: acc) tl
  in
  loop_stack [] lines
;;

(* Récupère le sommet de chaque pile *)
let get_top_crates stacks =
  Array.fold_left (fun acc stack ->
    match stack with
    | [] -> acc ^ " "
    | c :: _ -> acc ^ String.make 1 c
  ) "" stacks
;;

(* Fonction principale pour la partie 1 *)
let solve_part1 lines =
  let (stack_lines, instruction_lines) = split_stacks_and_instructions lines in
  let stacks = parse_stacks stack_lines in
  
  (* Applique toutes les instructions *)
  List.iter (fun line ->
    if String.trim line <> "" then begin
      let instruction = parse_instruction line in
      move stacks instruction
    end
  ) instruction_lines;
  
  get_top_crates stacks
;;

(* Fonction pour la partie 2 (à implémenter) *)
let solve_part2 lines =
  let (stack_lines, instruction_lines) = split_stacks_and_instructions lines in
  let stacks = parse_stacks stack_lines in
  
  (* Applique toutes les instructions avec le nouveau comportement *)
  List.iter (fun line ->
    if String.trim line <> "" then begin
      let instruction = parse_instruction line in
      move_part2 stacks instruction
    end
  ) instruction_lines;
  
  get_top_crates stacks
;;

let () =
  (* Appel du test unitaire *)
  Printf.printf "Test unitaire : %s\n" (test_parse_stacks());
  
  (* Résolution des parties 1 et 2 *)
  let lines = Utils.read_lines "input/day05.txt" in
  let result_part01 = solve_part1 lines in
  let result_part02 = solve_part2 lines in
  print_endline ("day05, part 1 : " ^ result_part01);
  print_endline ("day05, part 2 : " ^ result_part02);
;;