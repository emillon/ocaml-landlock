open Landlock

let setup_landlock () =
  let ruleset_attr : Ruleset.Attr.t =
    {
      handled_fs =
        [
          Execute;
          Write_file;
          Read_file;
          Read_dir;
          Remove_dir;
          Remove_file;
          Make_char;
          Make_dir;
          Make_reg;
          Make_sock;
          Make_fifo;
          Make_block;
          Make_sym;
          Refer;
          Truncate;
        ];
      handled_net = [ Bind_tcp; Connect_tcp ];
    }
  in
  let abi = Ruleset.get_abi () in
  let ruleset_attr = Ruleset.Attr.filter ruleset_attr ~abi in
  Ruleset.enforce_rules ruleset_attr
    ~rules:[ { parent = "/usr"; allowed_access = [ Read_file ] } ]

let can_read path =
  try
    In_channel.with_open_bin path (fun ic ->
        let _ = In_channel.input_byte ic in
        true)
  with Sys_error _ -> false

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
  check_read "/etc/hosts";
  check_write "/tmp/x"

let info = Cmdliner.Cmd.info "landlock"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
