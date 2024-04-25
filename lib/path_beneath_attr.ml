open Util

type 'a t = { parent : 'a; allowed_access : Access_fs.t list }

let open_dir path =
  let fd = C.Functions.open_ path C.Types.(o_path lor o_cloexec) in
  if fd < 0 then failwith "open_dir";
  int_to_fd fd

let with_open_dir path f =
  let fd = open_dir path in
  with_fd f fd

let with_opened p f =
  with_open_dir p.parent (fun fd -> f { p with parent = fd })
