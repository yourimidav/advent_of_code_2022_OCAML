(* day 21 *)

type monkey_job =
  | Number of int
  | Operation of string * char * string  (* monkey1, op, monkey2 *)

(* Parse une ligne *)
let parse_line line =
  match String.split_on_char ':' line with
  | [name; job] ->
      let job = String.trim job in
      let name = String.trim name in
      (match String.split_on_char ' ' job with
       | [num] -> (name, Number (int_of_string num))
       | [m1; op; m2] -> (name, Operation (m1, op.[0], m2))
       | _ -> failwith ("Invalid job: " ^ job))
  | _ -> failwith ("Invalid line: " ^ line)
;;

let parse_monkeys lines =
  List.map parse_line lines
;;

(* Map pour stocker les singes *)
module StringMap = Map.Make(String)

let build_monkey_map monkeys =
  List.fold_left (fun map (name, job) ->
    StringMap.add name job map
  ) StringMap.empty monkeys
;;

(* Évaluer récursivement ce qu'un singe crie *)
let rec eval monkey_map name =
  match StringMap.find name monkey_map with
  | Number n -> n
  | Operation (m1, op, m2) ->
      let v1 = eval monkey_map m1 in
      let v2 = eval monkey_map m2 in
      match op with
      | '+' -> v1 + v2
      | '-' -> v1 - v2
      | '*' -> v1 * v2
      | '/' -> v1 / v2
      | _ -> failwith ("Unknown operation: " ^ String.make 1 op)
;;

(* Partie 1 *)
let solve_part1 monkey_map =
  eval monkey_map "root"
;;

(* Partie 2 : trouver la valeur que "humn" doit crier *)

(* Vérifier si un singe dépend de "humn" *)
let rec depends_on_humn monkey_map name =
  if name = "humn" then true
  else
    match StringMap.find name monkey_map with
    | Number _ -> false
    | Operation (m1, _, m2) ->
        depends_on_humn monkey_map m1 || depends_on_humn monkey_map m2
;;

(* Résoudre l'équation inversée *)
let rec solve_for_humn monkey_map name target =
  if name = "humn" then target
  else
    match StringMap.find name monkey_map with
    | Number n -> 
        if n = target then target
        else failwith "Inconsistent equation"
    | Operation (m1, op, m2) ->
        (* Déterminer quelle branche contient humn *)
        let left_has_humn = depends_on_humn monkey_map m1 in
        
        if left_has_humn then begin
          (* humn est dans la branche gauche *)
          let right_value = eval monkey_map m2 in
          let new_target = match op with
            | '+' -> target - right_value  (* m1 + right = target => m1 = target - right *)
            | '-' -> target + right_value  (* m1 - right = target => m1 = target + right *)
            | '*' -> target / right_value  (* m1 * right = target => m1 = target / right *)
            | '/' -> target * right_value  (* m1 / right = target => m1 = target * right *)
            | _ -> failwith "Unknown op"
          in
          solve_for_humn monkey_map m1 new_target
        end else begin
          (* humn est dans la branche droite *)
          let left_value = eval monkey_map m1 in
          let new_target = match op with
            | '+' -> target - left_value   (* left + m2 = target => m2 = target - left *)
            | '-' -> left_value - target   (* left - m2 = target => m2 = left - target *)
            | '*' -> target / left_value   (* left * m2 = target => m2 = target / left *)
            | '/' -> left_value / target   (* left / m2 = target => m2 = left / target *)
            | _ -> failwith "Unknown op"
          in
          solve_for_humn monkey_map m2 new_target
        end
;;

let solve_part2 monkey_map =
  (* root doit avoir deux branches égales *)
  match StringMap.find "root" monkey_map with
  | Operation (m1, _, m2) ->
      (* Déterminer quelle branche contient humn *)
      let left_has_humn = depends_on_humn monkey_map m1 in
      
      if left_has_humn then begin
        (* On veut que m1 = eval(m2) *)
        let target = eval monkey_map m2 in
        Printf.printf "Target value: %d\n" target;
        solve_for_humn monkey_map m1 target
      end else begin
        (* On veut que m2 = eval(m1) *)
        let target = eval monkey_map m1 in
        Printf.printf "Target value: %d\n" target;
        solve_for_humn monkey_map m2 target
      end
  | _ -> failwith "root should be an operation"
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day21.txt" in
  let monkeys = parse_monkeys lines in
  let monkey_map = build_monkey_map monkeys in
  
  Printf.printf "Nombre de singes: %d\n" (List.length monkeys);

  let result_part1 = solve_part1 monkey_map in
  print_endline ("day21, part 1 : " ^ string_of_int result_part1);

  let result_part2 = solve_part2 monkey_map in
  print_endline ("day21, part 2 : " ^ string_of_int result_part2);
;;