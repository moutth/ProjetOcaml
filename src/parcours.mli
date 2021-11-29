open Graph

type 'a arc = id * id * 'a

val find_path: 'a graph -> id -> id -> ('a arc) list
