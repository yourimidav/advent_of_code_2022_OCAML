(*day 04*)

let container a b c d =
    let bool_first = (a <= c) && (b >= d) in
    let bool_second = (a >= c) && (b <= d) in
    bool_first || bool_second
;;


let parse_line line =
  let parts = String.split_on_char ',' line in
  let first, second = match parts with
    | [f; s] -> f, s
    | _ -> failwith "Format incorrect"
  in
  let a, b = match String.split_on_char '-' first with
    | [a; b] -> int_of_string a, int_of_string b
    | _ -> failwith "Format incorrect"
  in
  let c, d = match String.split_on_char '-' second with
    | [c; d] -> int_of_string c, int_of_string d
    | _ -> failwith "Format incorrect"
  in
  (a, b, c, d)
;;


let rec sum_container lines sum =
    match lines with
    | [] -> sum
    | hd::tl -> 
        let (a, b, c, d) = parse_line hd in
        if container a b c d then sum_container tl (sum + 1)
        else sum_container tl sum
    ;;

let overlap a b c d =
    if (a < c) then a <= d && c <= b
    else c <= b && a <= d
;;

let rec sum_overlap lines sum =
    match lines with
    | [] -> sum
    | hd::tl -> 
        let (a, b, c, d) = parse_line hd in
        if overlap a b c d then sum_overlap tl (sum + 1)
        else sum_overlap tl sum
    ;;

let () =
  let lines = Utils.read_lines "input/day04.txt" in
  let result_part01 = sum_container lines 0 in
  let result_part02 = sum_overlap lines 0 in
  print_endline ("day04, part 1 : " ^ string_of_int result_part01);
  print_endline ("day04, part 2 : " ^ string_of_int result_part02);

