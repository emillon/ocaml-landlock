module Ruleset_attr = struct
  module Access_fs = struct
    type t =
      | Execute
      | Write_file
      | Read_file
      | Read_dir
      | Remove_dir
      | Remove_file
      | Make_char
      | Make_dir
      | Make_reg
      | Make_sock
      | Make_fifo
      | Make_block
      | Make_sym
      | Refer
      | Truncate

    let to_int =
      let open C.Types in
      function
      | Execute -> landlock_access_fs_execute
      | Write_file -> landlock_access_fs_write_file
      | Read_file -> landlock_access_fs_read_file
      | Read_dir -> landlock_access_fs_read_dir
      | Remove_dir -> landlock_access_fs_remove_dir
      | Remove_file -> landlock_access_fs_remove_file
      | Make_char -> landlock_access_fs_make_char
      | Make_dir -> landlock_access_fs_make_dir
      | Make_reg -> landlock_access_fs_make_reg
      | Make_sock -> landlock_access_fs_make_sock
      | Make_fifo -> landlock_access_fs_make_fifo
      | Make_block -> landlock_access_fs_make_block
      | Make_sym -> landlock_access_fs_make_sym
      | Refer -> landlock_access_fs_refer
      | Truncate -> landlock_access_fs_truncate

    let supported_in ~abi = function
      | Refer -> abi >= 2
      | Truncate -> abi >= 3
      | Execute | Write_file | Read_file | Read_dir | Remove_dir | Remove_file
      | Make_char | Make_dir | Make_reg | Make_sock | Make_fifo | Make_block
      | Make_sym ->
          true
  end

  module Access_net = struct
    type t = Bind_tcp | Connect_tcp

    let to_int =
      let open C.Types in
      function
      | Bind_tcp -> landlock_access_net_bind_tcp
      | Connect_tcp -> landlock_access_net_connect_tcp

    let supported_in ~abi = function Bind_tcp | Connect_tcp -> abi >= 4
  end

  type t = {
    handled_access_fs : Access_fs.t list;
    handled_access_net : Access_net.t list;
  }

  let filter r ~abi =
    {
      handled_access_fs =
        List.filter (Access_fs.supported_in ~abi) r.handled_access_fs;
      handled_access_net =
        List.filter (Access_net.supported_in ~abi) r.handled_access_net;
    }

  let list_to_int to_int l = List.fold_left (fun acc a -> acc lor to_int a) 0 l

  let as_ptr { handled_access_fs; handled_access_net } =
    let open Ctypes in
    let p = allocate_n C.Types.ruleset_attr ~count:1 in
    p |-> C.Types.handled_access_fs
    <-@ list_to_int Access_fs.to_int handled_access_fs;
    p |-> C.Types.handled_access_net
    <-@ list_to_int Access_net.to_int handled_access_net;
    p
end

external setup_landlock_ : nativeint -> unit = "setup_landlock"

let get_abi () =
  C.Functions.landlock_create_ruleset C.Types.sys_landlock_create_ruleset
    Ctypes.null Unsigned.Size_t.zero C.Types.landlock_create_ruleset_version

let setup_landlock () =
  let ruleset_attr : Ruleset_attr.t =
    {
      handled_access_fs =
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
      handled_access_net = [ Bind_tcp; Connect_tcp ];
    }
  in
  let ruleset_attr = Ruleset_attr.filter ruleset_attr ~abi:(get_abi ()) in
  let ruleset_attr_ = Ruleset_attr.as_ptr ruleset_attr in
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
