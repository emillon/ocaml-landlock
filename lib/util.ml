let list_to_int to_int l = List.fold_left (fun acc a -> acc lor to_int a) 0 l
let int_to_fd : int -> Unix.file_descr = Obj.magic
let fd_to_int : Unix.file_descr -> int = Obj.magic

let with_fd f fd =
  Fun.protect (fun () -> f fd) ~finally:(fun () -> Unix.close fd)

let exec prog other_args =
  let argv = Array.of_list (prog :: other_args) in
  let fd = Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] fd in
  let n =
    match status with
    | WEXITED n -> n
    | WSIGNALED n ->
        Printf.eprintf "Process exited becaused of signal %d\n" n;
        127
    | WSTOPPED n ->
        Printf.eprintf "Process stopped (%d)\n" n;
        127
  in
  Stdlib.exit n
