(* day11.ml *)

type operation = 
  | Multiply of int
  | Add of int
  | Square

type monkey = {
  items: int list;
  operation: operation;
  divisor: int;
  true_target: int;
  false_target: int;
  inspections: int;
}

let apply_operation operation old =
  match operation with
  | Multiply n -> old * n
  | Add n -> old + n
  | Square -> old * old

let parse_monkey lines =
  let parse_items line =
    let colon_index = String.index line ':' in
    let items_str = String.sub line (colon_index + 1) (String.length line - colon_index - 1) in
    items_str 
    |> String.split_on_char ',' 
    |> List.map (fun s -> s |> String.trim |> int_of_string)
  in

  let parse_operation line =
    let parts = String.split_on_char ' ' line in
    match List.rev parts with
    | "old" :: "*" :: "old" :: _ -> Square
    | n :: "*" :: "old" :: _ -> Multiply (int_of_string n)
    | n :: "+" :: "old" :: _ -> Add (int_of_string n)
    | _ -> failwith ("Invalid operation: " ^ line)
  in

  let parse_last_int line =
    match List.rev (String.split_on_char ' ' line) with
    | n :: _ -> int_of_string n
    | _ -> failwith "Cannot parse integer from line"
  in

  match lines with
  | header :: items_line :: operation_line :: test_line :: true_line :: false_line :: _ ->
    if String.starts_with ~prefix:"Monkey" header then
      {
        items = parse_items items_line;
        operation = parse_operation operation_line;
        divisor = parse_last_int test_line;
        true_target = parse_last_int true_line;
        false_target = parse_last_int false_line;
        inspections = 0;
      }
    else
      failwith "Invalid monkey header"
  | _ -> failwith "Invalid monkey format"
;;

let process_monkey monkeys index worry_reduction modulus =
  let monkey = monkeys.(index) in
  if monkey.items = [] then monkeys (* Rien à traiter *)
  else
    let updated_monkeys = Array.copy monkeys in
  
    (* Traiter tous les objets du singe *)
    List.iter (fun item ->
      let new_worry = apply_operation monkey.operation item in
      let managed_worry = 
        if worry_reduction then 
          new_worry / 3 
        else 
          new_worry mod modulus
      in
      let target = 
        if managed_worry mod monkey.divisor = 0 then
          monkey.true_target
        else
          monkey.false_target
      in
      let target_monkey = updated_monkeys.(target) in
      updated_monkeys.(target) <- { 
        target_monkey with 
        items = target_monkey.items @ [managed_worry] 
      }
    ) monkey.items;
  
    (* Mettre à jour le singe courant *)
    updated_monkeys.(index) <- {
      monkey with
      items = [];
      inspections = monkey.inspections + List.length monkey.items;
    };
  
    updated_monkeys
;;

let simulate_round monkeys worry_reduction modulus =
  let rec process_all index current_monkeys =
    if index >= Array.length current_monkeys then
      current_monkeys
    else
      let updated = process_monkey current_monkeys index worry_reduction modulus in
      process_all (index + 1) updated
  in
  process_all 0 monkeys
;;

let simulate monkeys num_rounds worry_reduction =
  (* Calcul du modulus pour la partie 2 *)
  let modulus = 
    if worry_reduction then 
      max_int (* Pas besoin pour la partie 1 *)
    else
      Array.fold_left (fun acc monkey -> acc * monkey.divisor) 1 monkeys
  in
  
  let rec run_rounds round current_monkeys =
    if round >= num_rounds then
      current_monkeys
    else
      let next_monkeys = simulate_round current_monkeys worry_reduction modulus in
      run_rounds (round + 1) next_monkeys
  in
  run_rounds 0 monkeys
;;

let calculate_monkey_business monkeys =
  let inspections = 
    monkeys 
    |> Array.map (fun m -> m.inspections)
    |> Array.to_list
  in
  let sorted = List.sort (Fun.flip compare) inspections in
  match sorted with
  | a :: b :: _ -> a * b
  | _ -> failwith "Not enough monkeys"
;;

let solve_part1 monkeys =
  let final_monkeys = simulate (Array.copy monkeys) 20 true in
  calculate_monkey_business final_monkeys
;;

let solve_part2 monkeys =
  let final_monkeys = simulate (Array.copy monkeys) 10000 false in
  calculate_monkey_business final_monkeys
;;

let read_input filename =
  let lines = Utils.read_lines filename in
  (* Grouper par singes - chaque singe fait 6 lignes + 1 ligne vide *)
  let rec group_monkeys acc current_group = function
    | [] -> 
        if current_group <> [] then List.rev (List.rev current_group :: acc) 
        else List.rev acc
    | line :: rest ->
        if line = "" then
          group_monkeys (List.rev current_group :: acc) [] rest
        else
          group_monkeys acc (line :: current_group) rest
  in
  let groups = group_monkeys [] [] lines in
  groups |> List.map parse_monkey |> Array.of_list
;;

let () =
  let monkeys = read_input "input/day11.txt" in
  
  let result1 = solve_part1 monkeys in
  Printf.printf "Partie 1: %d\n" result1;
  
  let result2 = solve_part2 monkeys in
  Printf.printf "Partie 2: %d\n" result2
;;