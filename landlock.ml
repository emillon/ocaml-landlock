type access_fs =
  | LANDLOCK_ACCESS_FS_EXECUTE
  | LANDLOCK_ACCESS_FS_WRITE_FILE
  | LANDLOCK_ACCESS_FS_READ_FILE
  | LANDLOCK_ACCESS_FS_READ_DIR
  | LANDLOCK_ACCESS_FS_REMOVE_DIR
  | LANDLOCK_ACCESS_FS_REMOVE_FILE
  | LANDLOCK_ACCESS_FS_MAKE_CHAR
  | LANDLOCK_ACCESS_FS_MAKE_DIR
  | LANDLOCK_ACCESS_FS_MAKE_REG
  | LANDLOCK_ACCESS_FS_MAKE_SOCK
  | LANDLOCK_ACCESS_FS_MAKE_FIFO
  | LANDLOCK_ACCESS_FS_MAKE_BLOCK
  | LANDLOCK_ACCESS_FS_MAKE_SYM
  | LANDLOCK_ACCESS_FS_REFER
  | LANDLOCK_ACCESS_FS_TRUNCATE

type access_net =
  | LANDLOCK_ACCESS_NET_BIND_TCP
  | LANDLOCK_ACCESS_NET_CONNECT_TCP

type ruleset_attr = {
  handled_access_fs : access_fs list;
  handled_access_net : access_net list;
}

external setup_landlock_ : nativeint -> unit = "setup_landlock"

let access_fs_to_int = function
  | LANDLOCK_ACCESS_FS_EXECUTE -> C.Types.landlock_access_fs_execute
  | LANDLOCK_ACCESS_FS_WRITE_FILE -> C.Types.landlock_access_fs_write_file
  | LANDLOCK_ACCESS_FS_READ_FILE -> C.Types.landlock_access_fs_read_file
  | LANDLOCK_ACCESS_FS_READ_DIR -> C.Types.landlock_access_fs_read_dir
  | LANDLOCK_ACCESS_FS_REMOVE_DIR -> C.Types.landlock_access_fs_remove_dir
  | LANDLOCK_ACCESS_FS_REMOVE_FILE -> C.Types.landlock_access_fs_remove_file
  | LANDLOCK_ACCESS_FS_MAKE_CHAR -> C.Types.landlock_access_fs_make_char
  | LANDLOCK_ACCESS_FS_MAKE_DIR -> C.Types.landlock_access_fs_make_dir
  | LANDLOCK_ACCESS_FS_MAKE_REG -> C.Types.landlock_access_fs_make_reg
  | LANDLOCK_ACCESS_FS_MAKE_SOCK -> C.Types.landlock_access_fs_make_sock
  | LANDLOCK_ACCESS_FS_MAKE_FIFO -> C.Types.landlock_access_fs_make_fifo
  | LANDLOCK_ACCESS_FS_MAKE_BLOCK -> C.Types.landlock_access_fs_make_block
  | LANDLOCK_ACCESS_FS_MAKE_SYM -> C.Types.landlock_access_fs_make_sym
  | LANDLOCK_ACCESS_FS_REFER -> C.Types.landlock_access_fs_refer
  | LANDLOCK_ACCESS_FS_TRUNCATE -> C.Types.landlock_access_fs_truncate

let access_net_to_int = function
  | LANDLOCK_ACCESS_NET_BIND_TCP -> C.Types.landlock_access_net_bind_tcp
  | LANDLOCK_ACCESS_NET_CONNECT_TCP -> C.Types.landlock_access_net_connect_tcp

let access_list_to_int to_int l =
  List.fold_left (fun acc a -> acc lor to_int a) 0 l

let setup_landlock () =
  let ruleset_attr =
    {
      handled_access_fs =
        [
          LANDLOCK_ACCESS_FS_EXECUTE;
          LANDLOCK_ACCESS_FS_WRITE_FILE;
          LANDLOCK_ACCESS_FS_READ_FILE;
          LANDLOCK_ACCESS_FS_READ_DIR;
          LANDLOCK_ACCESS_FS_REMOVE_DIR;
          LANDLOCK_ACCESS_FS_REMOVE_FILE;
          LANDLOCK_ACCESS_FS_MAKE_CHAR;
          LANDLOCK_ACCESS_FS_MAKE_DIR;
          LANDLOCK_ACCESS_FS_MAKE_REG;
          LANDLOCK_ACCESS_FS_MAKE_SOCK;
          LANDLOCK_ACCESS_FS_MAKE_FIFO;
          LANDLOCK_ACCESS_FS_MAKE_BLOCK;
          LANDLOCK_ACCESS_FS_MAKE_SYM;
          LANDLOCK_ACCESS_FS_REFER;
          LANDLOCK_ACCESS_FS_TRUNCATE;
        ];
      handled_access_net =
        [ LANDLOCK_ACCESS_NET_BIND_TCP; LANDLOCK_ACCESS_NET_CONNECT_TCP ];
    }
  in
  let ruleset_attr_ =
    let open Ctypes in
    let p = allocate_n C.Types.ruleset_attr ~count:1 in
    p |-> C.Types.handled_access_fs
    <-@ access_list_to_int access_fs_to_int ruleset_attr.handled_access_fs;
    p |-> C.Types.handled_access_net
    <-@ access_list_to_int access_net_to_int ruleset_attr.handled_access_net;
    p
  in
  setup_landlock_ (Ctypes.raw_address_of_ptr (Ctypes.to_voidp ruleset_attr_))

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
