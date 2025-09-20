(* lib/utils.ml *)
let lines_of_string s =
  String.split_on_char '\n' s |> List.filter (fun l -> String.trim l <> "")
