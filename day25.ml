(* day 25 *)

(* Convertir un chiffre SNAFU en valeur *)
let snafu_digit_to_int = function
  | '2' -> 2
  | '1' -> 1
  | '0' -> 0
  | '-' -> -1
  | '=' -> -2
  | c -> failwith (Printf.sprintf "Invalid SNAFU digit: %c" c)
;;

(* Convertir un nombre SNAFU en décimal *)
let snafu_to_decimal snafu =
  let chars = String.to_seq snafu |> List.of_seq in
  List.fold_left (fun acc c ->
    acc * 5 + snafu_digit_to_int c
  ) 0 chars
;;

(* Convertir un entier en chiffre SNAFU *)
let int_to_snafu_digit n =
  match n with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '='  (* 3 en base 5 = -2 + 5 (carry) *)
  | 4 -> '-'  (* 4 en base 5 = -1 + 5 (carry) *)
  | _ -> failwith "Invalid digit"
;;

(* Convertir un nombre décimal en SNAFU *)
let decimal_to_snafu n =
  if n = 0 then "0"
  else
    let rec convert n acc =
      if n = 0 then acc
      else
        let digit = n mod 5 in
        let carry = if digit >= 3 then 1 else 0 in
        let snafu_digit = int_to_snafu_digit digit in
        convert ((n / 5) + carry) (String.make 1 snafu_digit ^ acc)
    in
    convert n ""
;;

(* Partie 1 : Additionner tous les nombres SNAFU *)
let solve_part1 lines =
  let total = List.fold_left (fun sum line ->
    let decimal = snafu_to_decimal line in
    sum + decimal
  ) 0 lines in
  
  Printf.printf "Total en décimal: %d\n" total;
  decimal_to_snafu total
;;

(* Tests *)
let test_conversions () =
  Printf.printf "=== Tests de conversion ===\n";
  
  let test_cases = [
    ("1", 1);
    ("2", 2);
    ("1=", 3);
    ("1-", 4);
    ("10", 5);
    ("11", 6);
    ("12", 7);
    ("2=", 8);
    ("2-", 9);
    ("20", 10);
    ("1=0", 15);
    ("1-0", 20);
    ("1=11-2", 2022);
    ("1-0---0", 12345);
    ("1121-1110-1=0", 314159265);
  ] in
  
  List.iter (fun (snafu, expected) ->
    let decimal = snafu_to_decimal snafu in
    let back = decimal_to_snafu decimal in
    Printf.printf "%s -> %d -> %s %s\n" 
      snafu decimal back 
      (if decimal = expected && back = snafu then "✓" else "✗")
  ) test_cases;
  
  Printf.printf "\n";
;;

(* Main *)
let () =
  (* Tests *)
  test_conversions ();
  
  (* Solution *)
  let lines = Utils.read_lines "input/day25.txt" in
  
  Printf.printf "Nombre de lignes: %d\n\n" (List.length lines);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 lines in
  print_endline ("day25, part 1 : " ^ result_part1);
;;