open Graph

type 'a arc = id * id * 'a



type 'a file = ('a) list

val enfiler: 'a file -> 'a -> 'a file

val pop: 'a file -> 'a

val empty_file: 'a file

val is_empty: 'a file -> bool 



val find_path: 'a graph -> id -> id -> ('a arc) list