open Graph
open Parcours
open Tools

let find_min graph path = 
  match path with
  | [] -> failwith "recherche de min sur chemin vide"
  | [hd] -> failwith "recherche de min sur chemin d'un seul noeud"
  | hd::(nxt::tl) -> 
    let rec parcours prev reste min = 
      match reste with
      | [] -> min
      | snd::suite -> 
        let lbl = find_arc graph prev snd in
        match lbl with
        | None -> failwith "arc sans label lors de recherche de min"
        | Some s -> let res = s in
          Printf.printf "prev=%d scd=%d min=%d res=%d%!\n" prev snd min res;
          if res > min then parcours snd suite min else parcours snd suite res
    in match find_arc graph hd nxt with
    | None -> failwith "arc inexistant dans le chemin donné"
    | Some x -> parcours hd (nxt::tl) x 

let update_graph graph path min =
  match path with
  | [] -> failwith "mise à jour du graph impossible : chemin vide"
  | [hd] -> failwith "mise à jour du graph impossible : chemin d'un seul noeud"
  | hd::(nxt::tl) -> 
    let rec parcours gr prev reste = 
      match reste with
      | [] -> gr
      | snd::suite -> parcours (add_arc (add_arc gr prev snd (-min)) snd prev min) snd suite
    in parcours graph hd (nxt::tl)


let rec ff20 graph source target max_flow =
  let path = find_path graph source target in
  if not (List.mem target path) then
    max_flow (* on s'arrete si aucun chemin nous reliant à la target n'existe *)
  else
    let min = find_min graph path in
    print_string (String.concat " " (List.map string_of_int path)); Printf.printf " min = %d%!\n" min;
    match min with 
    | 0 -> max_flow
    | x -> ff20 (update_graph graph path x) source target (max_flow+min)
