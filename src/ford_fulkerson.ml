open Graph
open Parcours

let find_min graph path = 
    match path with
        | [] -> failwith "ça marche pas c'est pas normal"
        | [hd] -> failwith "fais un effort mec"
        | hd::(nxt::tl) -> 
        let rec parcours prev reste min = match reste with
            | [] -> min
            | snd::suite -> 
                let lbl = find_arc graph prev snd in
                match lbl with
                    | None -> failwith "biip"
                    | Some s -> let res = int_of_string s in
                        if res > min then parcours snd suite min else parcours snd suite res
        in parcours hd tl (int_of_string (find_arc graph hd nxt))
