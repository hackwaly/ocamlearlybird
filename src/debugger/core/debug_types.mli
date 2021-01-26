type pc = int * int

type 'a source_location = { source : string; pos : int * int; end_ : 'a }

type source_position = unit source_location

type source_range = (int * int) source_location
