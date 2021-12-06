open Gfile
open Tools
open Parcours
open Ford_fulkerson

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in

  let () = export ("./graphs/" ^ (outfile ^ ".txt")) graph in

  let graph = gmap graph (int_of_string) in 

  let chemin = find_path graph _source _sink in
  Printf.printf "%s\n%!" (List.fold_left (fun a b -> a ^ " " ^ string_of_int b) "" chemin) ;

  let min = find_min graph chemin in 
  Printf.printf "%s\n%!" (string_of_int min)
  

  