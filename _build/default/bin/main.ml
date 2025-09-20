(* bin/main.ml *)

let read_all_file filename =
  let ic = open_in filename in
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n';
     done
   with End_of_file -> ());
  close_in ic;
  Buffer.contents buf

let () =
  let input = read_all_file "input/day01.txt" in
  Day01.run input

