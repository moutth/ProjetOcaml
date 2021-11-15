(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_iter (new_node gr)
let gmap gr f = assert false
let add_arc g id1 id2 n= assert false