(* day 20 *)

(* Parse les nombres *)
let parse_numbers lines =
  List.map int_of_string lines
;;

(* Représenter un élément avec son index original *)
type element = {
  value: int;
  original_index: int;
}

(* Créer la liste initiale avec les indices *)
let create_list numbers =
  List.mapi (fun i value -> { value; original_index = i }) numbers
;;

(* Déplacer un élément dans la liste *)
let mix_one list element =
  (* Trouver la position actuelle de l'élément *)
  let current_pos = ref 0 in
  List.iteri (fun i el ->
    if el.original_index = element.original_index then
      current_pos := i
  ) list;
  
  (* Retirer l'élément *)
  let without_element = List.filteri (fun i _ -> i <> !current_pos) list in
  let len = List.length without_element in
  
  if len = 0 then [element]
  else begin
    (* Calculer la nouvelle position (modulo pour la circularité) *)
    let move = element.value mod len in
    let new_pos = (!current_pos + move) mod len in
    let new_pos = if new_pos < 0 then new_pos + len else new_pos in
    
    (* Insérer à la nouvelle position *)
    let rec insert_at pos el lst acc idx =
      match lst with
      | [] -> List.rev (el :: acc)
      | hd :: tl ->
          if idx = pos then
            List.rev_append acc (el :: hd :: tl)
          else
            insert_at pos el tl (hd :: acc) (idx + 1)
    in
    
    insert_at new_pos element without_element [] 0
  end
;;

(* Mixer toute la liste une fois *)
let mix_once numbers list =
  List.fold_left (fun acc num ->
    let element = List.find (fun el -> el.original_index = num.original_index) acc in
    mix_one acc element
  ) list numbers
;;

(* Mixer n fois *)
let mix_n_times numbers list n =
  let rec loop acc count =
    if count = 0 then acc
    else begin
      if count mod 1 = 0 then
        Printf.printf "Mixing round %d/%d\n%!" (n - count + 1) n;
      loop (mix_once numbers acc) (count - 1)
    end
  in
  loop list n
;;

(* Trouver les coordonnées *)
let find_coordinates list =
  (* Trouver la position du 0 *)
  let zero_pos = ref 0 in
  List.iteri (fun i el ->
    if el.value = 0 then zero_pos := i
  ) list;
  
  let len = List.length list in
  let values = List.map (fun el -> el.value) list in
  
  (* Extraire les valeurs aux positions 1000, 2000, 3000 après le 0 *)
  let get_at offset =
    let pos = (!zero_pos + offset) mod len in
    List.nth values pos
  in
  
  let v1 = get_at 1000 in
  let v2 = get_at 2000 in
  let v3 = get_at 3000 in
  
  Printf.printf "Values at 1000, 2000, 3000: %d, %d, %d\n" v1 v2 v3;
  v1 + v2 + v3
;;

(* Partie 1 *)
let solve_part1 numbers =
  let list = create_list numbers in
  let mixed = mix_once (create_list numbers) list in
  find_coordinates mixed
;;

(* Partie 2 *)
let solve_part2 numbers =
  let decryption_key = 811589153 in
  let scaled_numbers = List.map (fun n -> n * decryption_key) numbers in
  let list = create_list scaled_numbers in
  let mixed = mix_n_times (create_list scaled_numbers) list 10 in
  find_coordinates mixed
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day20.txt" in
  let numbers = parse_numbers lines in
  
  Printf.printf "Nombre d'éléments: %d\n\n" (List.length numbers);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 numbers in
  print_endline ("day20, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2...\n";
  let result_part2 = solve_part2 numbers in
  print_endline ("day20, part 2 : " ^ string_of_int result_part2);
;;