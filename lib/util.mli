val fd_to_int : Unix.file_descr -> int
val int_to_fd : int -> Unix.file_descr
val list_to_int : ('a -> int) -> 'a list -> int
val with_fd : (Unix.file_descr -> 'a) -> Unix.file_descr -> 'a
val exec : string -> string list -> _
