open Graph
open Parcours
open Tools

let find_min graph path = 
    match path with
        | [] -> failwith "ça marche pas c'est pas normal"
        | [hd] -> failwith "fais un effort mec"
        | hd::(nxt::tl) -> 
        let rec parcours prev reste min = 
        match reste with
            | [] -> min
            | snd::suite -> 
                let lbl = find_arc graph prev snd in
                Printf.printf("prev=%d scd=%d min=%d%!\n") prev snd min ;
                match lbl with
                    | None -> failwith "biip"
                    | Some s -> let res = s in
                        if res > min then parcours snd suite min else parcours snd suite res
        in match find_arc graph hd nxt with
            | None -> failwith "biiiiiiip"
            | Some x -> parcours hd (nxt::tl) x 

let update_graph graph path min =
    match path with
        | [] -> failwith "ça marche pas c'est pas normal"
        | [hd] -> failwith "fais un effort mec"
        | hd::(nxt::tl) -> 
        let rec parcours gr prev reste = 
        match reste with
            | [] -> gr
            | snd::suite -> parcours ( add_arc (add_arc gr prev snd (-min)) snd prev min) snd suite
        in parcours graph hd (nxt::tl)