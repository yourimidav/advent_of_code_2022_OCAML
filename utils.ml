(* lib/utils.ml *)
(*let lines_of_string s =
  String.split_on_char '\n' s |> List.filter (fun l -> String.trim l <> "")*)

let read_lines filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None
  in
  let rec loop acc = match try_read () with
    | Some line -> loop (line :: acc)
    | None ->
      close_in ic;
      List.rev acc
  in
  loop []
