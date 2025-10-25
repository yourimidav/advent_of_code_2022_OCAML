(* day01/day01.ml *)


let rec group input result =
match input with
| []-> result
| "" :: rest -> group rest (0 :: result)
| cals :: rest -> 
  group rest
  (match result with
  |[] -> [int_of_string cals ]
  | hd :: tail -> (hd + int_of_string cals) :: tail)

let rec max_of_list input current = 
  match input with 
  | [] -> current
  | hd :: rest -> max_of_list rest (max hd current)

(*part 2*)

let rec max_of_3 input (m1, m2, m3) =
  match input with
  | [] -> (m1, m2, m3)
  | hd :: rest ->
    let new_m1, new_m2, new_m3 =
      if hd > m1 then (hd, m1, m2)
      else if hd > m2 then (m1, hd, m2)
      else if hd > m3 then (m1, m2, hd)
      else (m1, m2, m3)
    in
    max_of_3 rest (new_m1, new_m2, new_m3)

let () =
  let lines = Utils.read_lines "input/day01.txt" in
  let result = group lines [] in
  let max_calories = max_of_list result 0 in
  print_endline ("day01, part 1 : " ^ string_of_int max_calories);
  let m1, m2, m3 = max_of_3 result (0, 0, 0) in
  print_endline ("day01, part 2 : " ^ string_of_int (m1 + m2 + m3))
