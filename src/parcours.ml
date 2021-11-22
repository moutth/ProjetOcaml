open Graph

let empty_file = []

let is_empty f = match f with
  | [] -> true
  | _ -> false

let enfiler f e = append f [e]

let defiler f = match f with
  | [] -> []
  | hd::tl -> tl


let find_path graph src target =
  if not (node_exists graph src && node_exists graph target) then raise (Graph_error ("A node does not exist in the graph."))
  else
    let visited = [src] in
    let rec parcours graph path target out_list =
      match out_list with
      | [(node, _) :: reste] -> 
        if not(mem node visited) then
          append visited [node];
        if node = target then
          [node :: path]
        else
          parcours graph [node :: path] target (out_arcs graph node);
        parcours graph path target reste
      | [(node, _)] ->
        if not(mem node visited) then 
          append visited [node];
        if node = target then
          [node :: path]
        else
          parcours graph [node :: path] target (out_arcs graph node)
      | _ -> []