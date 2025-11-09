(* day 07 *)
type line_type =
  | Command of string * string option
  | DirEntry of string
  | FileEntry of string * int

type parse_state = {
  current_path: string list;
  files: (string * int) list;
}

let parse_line line =
  let parts = String.split_on_char ' ' line in
  match parts with
  | "$" :: "cd" :: dir :: [] -> Command ("cd", Some dir)
  | "$" :: "ls" :: [] -> Command ("ls", None)
  | "dir" :: name :: [] -> DirEntry name
  | size :: name :: [] ->
      (try FileEntry (name, int_of_string size)
       with _ -> failwith ("Parse error: " ^ line))
  | _ -> failwith ("Unknown line format: " ^ line)

let build_full_path current_path filename =
  if current_path = [] then "/" ^ filename
  else "/" ^ (String.concat "/" current_path) ^ "/" ^ filename
;;

(* Ajoute cette fonction helper au début de ton fichier *)
let remove_last lst =
  match List.rev lst with
  | [] -> []
  | _ :: tl -> List.rev tl
;;


let process_line state line =
  match parse_line line with
  | Command ("cd", Some "/") ->
      Printf.printf "[CD /] current_path: [] → []\n";
      { state with current_path = [] }
  | Command ("cd", Some "..") ->
      { state with current_path = remove_last state.current_path }
  | Command ("cd", Some dir) ->
      let new_path = state.current_path @ [dir] in
      Printf.printf "[CD %s] current_path: %s → %s\n" 
        dir
        (String.concat "/" state.current_path) 
        (String.concat "/" new_path);
      { state with current_path = new_path }
  | Command ("ls", None) ->
      state
  | DirEntry _ ->
      state
  | FileEntry (name, size) ->
      let full_path = build_full_path state.current_path name in
      Printf.printf "[FILE] %s (in %s)\n" full_path (String.concat "/" state.current_path);
      { state with files = (full_path, size) :: state.files }
  | _ -> state
  ;;

let parse_all_lines lines =
  let initial_state = { current_path = []; files = [] } in
  List.fold_left process_line initial_state lines
;;

let get_parent_dirs path =
  let parts = String.split_on_char '/' path 
              |> List.filter (fun s -> s <> "") in
  let rec build_dirs acc curr_path = function
    | [] | [_] -> acc  (* Arrêt quand il ne reste que le fichier ou rien *)
    | hd :: tl ->
        let new_path = if curr_path = "" then "/" ^ hd else curr_path ^ "/" ^ hd in
        build_dirs (new_path :: acc) new_path tl
  in
  List.rev ("/" :: build_dirs [] "" parts)
;;

let get_all_dirs files =
  files
  |> List.map fst
  |> List.map get_parent_dirs
  |> List.flatten
  |> List.sort_uniq String.compare
;;

let calc_dir_size dir_path files =
  List.filter (fun (path, _) ->
    if dir_path = "/" then true
    else String.starts_with ~prefix:(dir_path ^ "/") path
  ) files
  |> List.map snd
  |> List.fold_left (+) 0
;;


let solve_part1 files =
  let dirs = get_all_dirs files in
  let dir_sizes = List.map (fun dir -> calc_dir_size dir files) dirs in
  List.filter (fun size -> size <= 100000) dir_sizes
  |> List.fold_left (+) 0
;;

let solve_part2 files =
  let total_space = 70000000 in
  let needed_space = 30000000 in
  let used_space = calc_dir_size "/" files in
  let free_space = total_space - used_space in
  let to_free = needed_space - free_space in
  if to_free <= 0 then 0
  else
    let dirs = get_all_dirs files in
    let dir_sizes = List.map (fun dir -> (dir, calc_dir_size dir files)) dirs in
    List.filter (fun (_, size) -> size >= to_free) dir_sizes
    |> List.map snd
    |> List.sort compare
    |> List.hd

    

let () =
  let lines = Utils.read_lines "input/day07.txt" in
  
  (* DEBUG : Afficher les 20 premières lignes *)
  Printf.printf "=== 20 premières lignes du fichier ===\n";
  List.iteri (fun i line ->
    if i < 20 then Printf.printf "%d: %s\n" i line
  ) lines;
  Printf.printf "\n";
  
  let state = parse_all_lines lines in
  let files = state.files in
  
  (* DEBUG : Afficher quelques fichiers *)
  let rec take n lst =
    match n, lst with
    | 0, _ | _, [] -> []
    | n, h :: t -> h :: take (n-1) t
  in
  Printf.printf "=== Premiers fichiers parsés ===\n";
  List.iter (fun (path, size) ->
    Printf.printf "%s -> %d\n" path size
  ) (take 10 (List.rev files));
  
  let result_part01 = solve_part1 files in
  let result_part02 = solve_part2 files in
  
  print_endline ("\nday07, part 1 : " ^ string_of_int result_part01);
  print_endline ("day07, part 2 : " ^ string_of_int result_part02);