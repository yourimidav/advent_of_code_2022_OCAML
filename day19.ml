(* day 19 *)

type resources = {
  ore: int;
  clay: int;
  obsidian: int;
  geode: int;
}

type robots = {
  ore_robots: int;
  clay_robots: int;
  obsidian_robots: int;
  geode_robots: int;
}

type blueprint = {
  id: int;
  ore_robot_cost: int;  (* en ore *)
  clay_robot_cost: int;  (* en ore *)
  obsidian_robot_cost: int * int;  (* ore, clay *)
  geode_robot_cost: int * int;  (* ore, obsidian *)
}

(* Parse une ligne *)
let parse_line line =
  let pattern = Str.regexp 
    "Blueprint \\([0-9]+\\): Each ore robot costs \\([0-9]+\\) ore\\. Each clay robot costs \\([0-9]+\\) ore\\. Each obsidian robot costs \\([0-9]+\\) ore and \\([0-9]+\\) clay\\. Each geode robot costs \\([0-9]+\\) ore and \\([0-9]+\\) obsidian\\." in
  
  if Str.string_match pattern line 0 then
    let id = int_of_string (Str.matched_group 1 line) in
    let ore_robot_cost = int_of_string (Str.matched_group 2 line) in
    let clay_robot_cost = int_of_string (Str.matched_group 3 line) in
    let obsidian_ore = int_of_string (Str.matched_group 4 line) in
    let obsidian_clay = int_of_string (Str.matched_group 5 line) in
    let geode_ore = int_of_string (Str.matched_group 6 line) in
    let geode_obsidian = int_of_string (Str.matched_group 7 line) in
    {
      id;
      ore_robot_cost;
      clay_robot_cost;
      obsidian_robot_cost = (obsidian_ore, obsidian_clay);
      geode_robot_cost = (geode_ore, geode_obsidian);
    }
  else
    failwith ("Failed to parse: " ^ line)
;;

let parse_blueprints lines =
  List.map parse_line lines
;;

(* Créer des ressources/robots initiaux *)
let empty_resources = { ore = 0; clay = 0; obsidian = 0; geode = 0 }
let initial_robots = { ore_robots = 1; clay_robots = 0; obsidian_robots = 0; geode_robots = 0 }

(* Collecter les ressources *)
let collect resources robots = {
  ore = resources.ore + robots.ore_robots;
  clay = resources.clay + robots.clay_robots;
  obsidian = resources.obsidian + robots.obsidian_robots;
  geode = resources.geode + robots.geode_robots;
}

(* Calculer les max de chaque ressource nécessaire par minute *)
let max_costs blueprint = 
  let max_ore = max blueprint.ore_robot_cost 
    (max blueprint.clay_robot_cost 
      (max (fst blueprint.obsidian_robot_cost) (fst blueprint.geode_robot_cost))) in
  let max_clay = snd blueprint.obsidian_robot_cost in
  let max_obsidian = snd blueprint.geode_robot_cost in
  (max_ore, max_clay, max_obsidian)
;;

(* DFS avec élagage *)
let find_max_geodes blueprint time_limit =
  let (max_ore, max_clay, max_obsidian) = max_costs blueprint in
  let best = ref 0 in
  
  let rec dfs time resources robots =
    if time = 0 then begin
      best := max !best resources.geode;
    end else begin
      (* Élagage : estimation optimiste *)
      let remaining = time in
      let potential_geodes = 
        resources.geode + 
        (robots.geode_robots * remaining) +
        (remaining * (remaining - 1) / 2) in
      
      if potential_geodes <= !best then ()
      else begin
        let new_resources = collect resources robots in
        
        (* Option 1 : Construire un robot geode *)
        let (geode_ore, geode_obs) = blueprint.geode_robot_cost in
        if resources.ore >= geode_ore && resources.obsidian >= geode_obs then begin
          let res = { new_resources with 
            ore = new_resources.ore - geode_ore;
            obsidian = new_resources.obsidian - geode_obs 
          } in
          let rob = { robots with geode_robots = robots.geode_robots + 1 } in
          dfs (time - 1) res rob
        end;
        
        (* Option 2 : Construire un robot obsidian *)
        if robots.obsidian_robots < max_obsidian then begin
          let (obs_ore, obs_clay) = blueprint.obsidian_robot_cost in
          if resources.ore >= obs_ore && resources.clay >= obs_clay then begin
            let res = { new_resources with 
              ore = new_resources.ore - obs_ore;
              clay = new_resources.clay - obs_clay 
            } in
            let rob = { robots with obsidian_robots = robots.obsidian_robots + 1 } in
            dfs (time - 1) res rob
          end
        end;
        
        (* Option 3 : Construire un robot clay *)
        if robots.clay_robots < max_clay then begin
          if resources.ore >= blueprint.clay_robot_cost then begin
            let res = { new_resources with 
              ore = new_resources.ore - blueprint.clay_robot_cost 
            } in
            let rob = { robots with clay_robots = robots.clay_robots + 1 } in
            dfs (time - 1) res rob
          end
        end;
        
        (* Option 4 : Construire un robot ore *)
        if robots.ore_robots < max_ore then begin
          if resources.ore >= blueprint.ore_robot_cost then begin
            let res = { new_resources with 
              ore = new_resources.ore - blueprint.ore_robot_cost 
            } in
            let rob = { robots with ore_robots = robots.ore_robots + 1 } in
            dfs (time - 1) res rob
          end
        end;
        
        (* Option 5 : Ne rien construire *)
        dfs (time - 1) new_resources robots
      end
    end
  in
  
  dfs time_limit empty_resources initial_robots;
  !best
;;

(* Partie 1 *)
let solve_part1 blueprints =
  List.fold_left (fun total bp ->
    Printf.printf "Blueprint %d...\n%!" bp.id;
    let geodes = find_max_geodes bp 24 in
    Printf.printf "  Max geodes: %d\n%!" geodes;
    total + (bp.id * geodes)
  ) 0 blueprints
;;

(* Partie 2 *)
let solve_part2 blueprints =
  let first_three = 
    if List.length blueprints > 3 then
      [List.nth blueprints 0; List.nth blueprints 1; List.nth blueprints 2]
    else
      blueprints
  in
  List.fold_left (fun product bp ->
    Printf.printf "Blueprint %d (32 min)...\n%!" bp.id;
    let geodes = find_max_geodes bp 32 in
    Printf.printf "  Max geodes: %d\n%!" geodes;
    product * geodes
  ) 1 first_three
;;

(* Main *)
let () =
  let lines = Utils.read_lines "input/day19.txt" in
  let blueprints = parse_blueprints lines in
  
  Printf.printf "Nombre de blueprints: %d\n\n" (List.length blueprints);
  
  Printf.printf "Partie 1...\n";
  let result_part1 = solve_part1 blueprints in
  print_endline ("day19, part 1 : " ^ string_of_int result_part1);
  
  Printf.printf "\nPartie 2...\n";
  let result_part2 = solve_part2 blueprints in
  print_endline ("day19, part 2 : " ^ string_of_int result_part2);
;;