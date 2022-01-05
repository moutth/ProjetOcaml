open Graph


let find_path graph src target =
  if not (node_exists graph src && node_exists graph target) then
    raise (Graph_error ("A node does not exist in the graph."))
  else
    let rec explore current_node path out_list visited =
      match out_list with
      | [] -> []
      | (next_node,_)::reste -> 
        if (List.mem next_node visited) || ((find_arc graph current_node next_node) <= Some(0)) then
          explore current_node path reste visited
        else if next_node = target then
          List.append [next_node;current_node] path
        else
          let chemin = explore next_node (current_node::path) (out_arcs graph next_node) (current_node::visited) in
          if chemin <> [] then
            chemin
          else
            explore current_node path reste (List.append [current_node;next_node] visited)
    in List.rev (explore src [] (out_arcs graph src) [])
