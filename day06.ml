(*day 06*)

(* Extraire N caractères à partir de la position i *)
let get_n_chars s i n =
  let rec aux pos acc count =
    if count = 0 then List.rev acc
    else aux (pos + 1) (s.[pos] :: acc) (count - 1)
  in
  aux i [] n
;;

let rec is_all_diff list_char =
  match list_char with
  | [] -> true
  | hd::tl -> 
    if tl<>[] && List.mem hd tl then false 
    else is_all_diff tl
  ;;

let rec result01 line index =
  if index > String.length line then failwith ("pas de marqueur")
  else 
    let list_char = get_n_chars line index 4 in 
    if is_all_diff list_char then index + 4
    else result01 line (index + 1)
  ;;

  let rec result02 line index =
  if index > String.length line then failwith ("pas de marqueur")
  else 
    let list_char = get_n_chars line index 14 in 
    if is_all_diff list_char then index + 14
    else result02 line (index + 1)
  ;;

let () =
  let lines = Utils.read_lines "input/day06.txt" in
  let index01 = result01 (List.hd lines) 0 in 
  let index02 = result02 (List.hd lines) 0 in 
  print_endline ("day06, part 1 : " ^ string_of_int index01);
  print_endline ("day06, part 2 : " ^ string_of_int index02);
