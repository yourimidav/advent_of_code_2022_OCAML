(*day 10*)

type instruction =
  | Noop
  | Addx of int
;;

let parse_instruction line =
  match String.split_on_char ' ' line with
  | ["noop"] -> Noop
  | ["addx"; value] -> Addx (int_of_string value)
  | _ -> failwith ("Invalid instruction: " ^ line)
;;

let simulate instructions =
  let x_values = ref [] in  (* Liste vide au départ *)
  let current_x = ref 1 in
  let rec process = function
  | [] -> ()
  | instr :: tl ->
    match instr with
    | Noop ->
      x_values := !current_x :: !x_values;
      process tl
    | Addx v ->
      (* Premier cycle : valeur actuelle de X *)
      x_values := !current_x :: !x_values;
      (* Deuxième cycle : valeur actuelle de X *)
      x_values := !current_x :: !x_values;
      (* Mise à jour de X après le deuxième cycle *)
      current_x := !current_x + v;
      process tl
  in
  process instructions;
  List.rev !x_values
;;

let solve_part1 instructions =
  let x_values = simulate instructions in
  let interesting_cycles = [20; 60; 100; 140; 180; 220] in
  List.fold_left (fun acc cycle ->
    let x = List.nth x_values (cycle - 1) in
    acc + (cycle * x)
  ) 0 interesting_cycles
;;

let solve_part2 instructions =
  let x_values = simulate instructions in
  let crt = Array.make_matrix 6 40 '.' in
  for cycle = 0 to 239 do
    let x = List.nth x_values cycle in
    let row = cycle / 40 in
    let col = cycle mod 40 in
    if abs (x - col) <= 1 then
      crt.(row).(col) <- '#'
  done;
  for row = 0 to 5 do
    for col = 0 to 39 do
      print_char crt.(row).(col)
    done;
    print_newline ()
  done
;;

let () =
  let lines = Utils.read_lines "input/day10.txt" in
  let instructions = List.map parse_instruction lines in
  let result_part1 = solve_part1 instructions in
  Printf.printf "Partie 1: %d\n" result_part1;
  Printf.printf "Partie 2:\n";
  solve_part2 instructions;
