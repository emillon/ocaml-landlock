external setup_landlock : unit -> unit = "setup_landlock"

let can_read path =
  let ok = ref false in
  In_channel.with_open_bin path (fun _ic -> ok := true);
  !ok

let can_write path =
  try Out_channel.with_open_bin path (fun _ic -> true)
  with Sys_error _ -> false

let check_read path = Printf.printf "can read %s: %b\n" path (can_read path)
let check_write path = Printf.printf "can write %s: %b\n" path (can_write path)

let term =
  let ( let+ ) x f = Cmdliner.Term.(const f $ x) in
  let+ restrict =
    Cmdliner.Arg.value (Cmdliner.Arg.flag (Cmdliner.Arg.info [ "restrict" ]))
  in
  if restrict then setup_landlock ();
  check_read "/usr/include/paths.h";
  check_read "/bin/bash";
  check_write "/tmp/x"

let info = Cmdliner.Cmd.info "landlock"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
