(*day 03*)

let compare_line_part1 line =
    let length_line = String.length line in
    let first_part = String.sub line 0 (length_line / 2) in
    let second_part = String.sub line (length_line / 2) (length_line / 2) in

    (* Fonction locale pour trouver le premier caractère commun *)
    let find_common_char part1 part2 =
        let len = String.length part1 in
        let rec aux i =
            if i >= len then None
            else if String.contains part2 part1.[i] then Some part1.[i]
            else aux (i + 1)
        in
        aux 0
    in

    find_common_char first_part second_part
    

let getPrioritySum letter sum =
    let ascii = int_of_char letter in 
    let newsum =
        if ascii >= 65 && ascii <= 90 then 
            sum + ascii - 38
        else sum + ascii - 96
    in newsum;;

let rec result lines sum =
    match lines with
    | [] -> sum
    | hd::tl ->
      match compare_line_part1 hd with
        | Some c -> result tl (getPrioritySum c sum)
        | None   -> result tl sum

let contains_in_two_lines letter line1 line2 =
    String.contains line1 letter && String.contains line2 letter



let rec compare_lines_three_by_three lines sum =
    match lines with
    | [] -> sum
    | line1::line2::line3::tl ->
        let rec aux i =
            if i >= String.length line1 then
                None
            else if contains_in_two_lines line1.[i] line2 line3 then
                Some line1.[i]
            else
                aux (i + 1)
        in
        let newsum =
            match aux 0 with
            | Some c -> getPrioritySum c sum
            | None -> sum
        in
        compare_lines_three_by_three tl newsum
    | _ -> sum



let () =
  let lines = Utils.read_lines "input/day03.txt" in
  let result_part01 = result lines 0 in
  let result_part02 = compare_lines_three_by_three lines 0 in 
  print_endline ("day03, part 1 : " ^ string_of_int result_part01);
  print_endline ("day03, part 2 : " ^ string_of_int result_part02);


