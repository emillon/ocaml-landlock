type 'a t = { parent : 'a; allowed_access : Access_fs.t list }

val with_opened : string t -> (Unix.file_descr t -> 'a) -> 'a
