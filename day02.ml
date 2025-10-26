(* day02 *)

(* il faut que je parcours les input *)

let oneScore line =
  match line with
  | "A X" ->  4
  | "A Y" ->  8
  | "A Z" ->  3
  | "B X" ->  1
  | "B Y" ->  5
  | "B Z" ->  9
  | "C X" ->  7
  | "C Y" ->  2
  | "C Z" ->  6
  | _ -> 0

let rec somme sum lines scoring=
  match lines with
  | [] -> sum
  | hd::tl ->
      if hd = "" then sum
      else somme (sum + scoring hd) tl scoring

let oneScore_part2 line =
  match line with
  | "A X" ->  3
  | "A Y" ->  4
  | "A Z" ->  8
  | "B X" ->  1
  | "B Y" ->  5
  | "B Z" ->  9
  | "C X" ->  2
  | "C Y" ->  6
  | "C Z" ->  7
  | _ -> 0

let () =
  let lines = Utils.read_lines "input/day02.txt" in
  let somme_total_part1 = somme 0 lines oneScore in
  print_endline ("day02, part 1 : " ^ string_of_int somme_total_part1);
  let somme_total_part2 = somme 0 lines oneScore_part2 in
  print_endline ("day02, part 2 : " ^ string_of_int somme_total_part2)
