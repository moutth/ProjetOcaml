(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph
let gmap gr f = assert false
let add_arc g id1 id2 n =
  let lbl = find_arc g id1 id2 in
  match lbl with
  | Some(x) -> new_arc g id1 id2 (x + n)
  | None -> new_arc g id1 id2 n
  
