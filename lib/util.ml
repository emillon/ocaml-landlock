let list_to_int to_int l = List.fold_left (fun acc a -> acc lor to_int a) 0 l
let int_to_fd : int -> Unix.file_descr = Obj.magic
let fd_to_int : Unix.file_descr -> int = Obj.magic

let with_fd f fd =
  Fun.protect (fun () -> f fd) ~finally:(fun () -> Unix.close fd)
