(* day 13*)

type packet =
  | Int of int
  | List of packet list
;;

(* Enlève les espaces *)
let rec skip_spaces s pos =
  if pos >= String.length s then pos
  else if s.[pos] = ' ' then skip_spaces s (pos + 1)
  else pos
;;

(* Parse un entier *)
let parse_int s pos =
  let rec loop acc pos =
    if pos >= String.length s then (acc, pos)
    else match s.[pos] with
      | '0'..'9' as c -> 
          loop (acc * 10 + (int_of_char c - int_of_char '0')) (pos + 1)
      | _ -> (acc, pos)
  in
  loop 0 pos
;;

(* Parse un packet (récursif) *)
let rec parse_packet s pos =
  let pos = skip_spaces s pos in
  if pos >= String.length s then failwith "Unexpected end of input"
  else if s.[pos] = '[' then
    parse_list s (pos + 1)
  else if s.[pos] >= '0' && s.[pos] <= '9' then
    let (n, new_pos) = parse_int s pos in
    (Int n, new_pos)
  else
    failwith ("Unexpected character: " ^ String.make 1 s.[pos])

(* Parse une liste *)
and parse_list s pos =
  let pos = skip_spaces s pos in
  if pos >= String.length s then failwith "Unexpected end of list"
  else if s.[pos] = ']' then
    (List [], pos + 1)  (* Liste vide *)
  else
    let rec parse_elements acc pos =
      let pos = skip_spaces s pos in
      let (elem, new_pos) = parse_packet s pos in
      let acc = elem :: acc in
      let new_pos = skip_spaces s new_pos in
      if new_pos >= String.length s then failwith "Unexpected end"
      else if s.[new_pos] = ']' then
        (List (List.rev acc), new_pos + 1)
      else if s.[new_pos] = ',' then
        parse_elements acc (new_pos + 1)
      else
        failwith ("Expected ',' or ']', got: " ^ String.make 1 s.[new_pos])
    in
    parse_elements [] pos
  ;;

(* Parse une ligne complète *)
let parse_line line =
  let (packet, _) = parse_packet line 0 in
  packet
;;

(* Parse les paires (séparées par des lignes vides) *)
let parse_pairs lines =
  let rec loop acc current = function
    | [] -> 
        (match current with
         | [p2; p1] -> List.rev ((p1, p2) :: acc)  (* Inverse car on accumule *)
         | _ -> List.rev acc)
    | line :: rest ->
        if String.trim line = "" then
          match current with
          | [p2; p1] -> loop ((p1, p2) :: acc) [] rest
          | _ -> loop acc [] rest
        else
          let packet = parse_line line in
          loop acc (packet :: current) rest
  in
  loop [] [] lines
;;

(* Fonction pour afficher un packet *)
let rec string_of_packet = function
  | Int n -> string_of_int n
  | List lst -> 
      "[" ^ String.concat "," (List.map string_of_packet lst) ^ "]"
;;

(* Compare deux packets selon les règles du jour 13 *)
let rec compare_packets left right =
  match (left, right) with
  (* Cas 1 : Deux entiers *)
  | (Int l, Int r) ->
      if l < r then -1
      else if l > r then 1
      else 0
  
  (* Cas 2 : Deux listes *)
  | (List l, List r) ->
      compare_lists l r
  
  (* Cas 3 : Entier à gauche, liste à droite *)
  | (Int l, List _) ->
      compare_packets (List [Int l]) right
  
  (* Cas 4 : Liste à gauche, entier à droite *)
  | (List _, Int r) ->
      compare_packets left (List [Int r])

(* Compare deux listes élément par élément *)
and compare_lists left right =
  match (left, right) with
  (* Les deux listes sont vides : égalité *)
  | ([], []) -> 0
  
  (* Liste gauche vide, droite non vide : gauche plus petite *)
  | ([], _) -> -1
  
  (* Liste droite vide, gauche non vide : gauche plus grande *)
  | (_, []) -> 1
  
  (* Comparer les premiers éléments *)
  | (l :: l_rest, r :: r_rest) ->
      let cmp = compare_packets l r in
      if cmp <> 0 then cmp  (* Si différents, on a la réponse *)
      else compare_lists l_rest r_rest  (* Sinon, continuer avec le reste *)
;;

let solve_part1 pairs =
  List.mapi (fun i (left, right) ->
    let cmp = compare_packets left right in
    if cmp <= 0 then i + 1 else 0  (* Index commence à 1 *)
  ) pairs
  |> List.fold_left (+) 0
;;

let solve_part2 pairs =
  (* Aplatir toutes les paires en une seule liste *)
  let all_packets = List.fold_left (fun acc (left, right) ->
    left :: right :: acc
  ) [] pairs in
  
  (* Ajouter les dividers [[2]] et [[6]] *)
  let divider1 = List [List [Int 2]] in
  let divider2 = List [List [Int 6]] in
  let all_with_dividers = divider1 :: divider2 :: all_packets in
  
  (* Trier *)
  let sorted = List.sort compare_packets all_with_dividers in
  
  (* Trouver les indices des dividers (commence à 1) *)
  let find_index packet lst =
    let rec loop i = function
      | [] -> failwith "Divider not found"
      | hd :: tl ->
          if compare_packets hd packet = 0 then i
          else loop (i + 1) tl
    in
    loop 1 lst
  in
  
  let idx1 = find_index divider1 sorted in
  let idx2 = find_index divider2 sorted in
  idx1 * idx2
;;

(* Test *)
let () =
  let test_lines = [
    "[1,1,3,1,1]";
    "[1,1,5,1,1]";
    "";
    "[[1],[2,3,4]]";
    "[[1],4]";
  ] in
  let pairs = parse_pairs test_lines in
  List.iteri (fun i (p1, p2) ->
    Printf.printf "Paire %d:\n" (i + 1);
    Printf.printf "  Gauche: %s\n" (string_of_packet p1);
    Printf.printf "  Droite: %s\n" (string_of_packet p2);
  ) pairs
;;


let () =
  let lines = Utils.read_lines "input/day13.txt" in
  let pairs = parse_pairs lines in
  
  let result_part1 = solve_part1 pairs in
  let result_part2 = solve_part2 pairs in
  
  print_endline ("day13, part 1 : " ^ string_of_int result_part1);
  print_endline ("day13, part 2 : " ^ string_of_int result_part2);


