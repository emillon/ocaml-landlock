let list_to_int to_int l = List.fold_left (fun acc a -> acc lor to_int a) 0 l
let int_to_fd : int -> Unix.file_descr = Obj.magic
let fd_to_int : Unix.file_descr -> int = Obj.magic

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

module Ruleset_attr = struct
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

  let as_ptr { handled_access_fs; handled_access_net } =
    let open Ctypes in
    let p = allocate_n C.Types.ruleset_attr ~count:1 in
    p |-> C.Types.handled_access_fs
    <-@ list_to_int Access_fs.to_int handled_access_fs;
    p |-> C.Types.handled_access_net
    <-@ list_to_int Access_net.to_int handled_access_net;
    p
end

module Path_beneath_attr = struct
  type t = { allowed_access : Access_fs.t list; parent_fd : Unix.file_descr }

  let as_ptr { allowed_access; parent_fd } =
    let open Ctypes in
    let p = allocate_n C.Types.path_beneath_attr ~count:1 in
    p |-> C.Types.allowed_access <-@ list_to_int Access_fs.to_int allowed_access;
    p |-> C.Types.parent_fd <-@ fd_to_int parent_fd;
    p
end

let get_abi () =
  C.Functions.landlock_create_ruleset C.Types.sys_landlock_create_ruleset
    Ctypes.null Unsigned.Size_t.zero C.Types.landlock_create_ruleset_version

let create_ruleset ruleset_attr =
  let p_ruleset_attr = ruleset_attr |> Ruleset_attr.as_ptr |> Ctypes.to_voidp in
  C.Functions.landlock_create_ruleset C.Types.sys_landlock_create_ruleset
    p_ruleset_attr
    (Unsigned.Size_t.of_int (Ctypes.sizeof C.Types.ruleset_attr))
    Unsigned.UInt32.zero
  |> int_to_fd

let restrict_self ruleset_fd =
  let err =
    C.Functions.landlock_restrict_self C.Types.sys_landlock_restrict_self
      (fd_to_int ruleset_fd) 0
  in
  if err <> 0 then failwith "landlock_restrict_self"

let no_new_privs () =
  let err = C.Functions.prctl C.Types.pr_set_no_new_privs 1 0 0 0 in
  if err <> 0 then failwith "prctl"

let open_dir path =
  let fd = C.Functions.open_ path C.Types.(o_path lor o_cloexec) in
  if fd < 0 then failwith "open_dir";
  int_to_fd fd

let add_rule ruleset_fd path_beneath =
  let p_path_beneath =
    Path_beneath_attr.as_ptr path_beneath |> Ctypes.to_voidp
  in
  let err =
    C.Functions.landlock_add_rule C.Types.sys_landlock_add_rule
      (fd_to_int ruleset_fd) C.Types.landlock_rule_path_beneath p_path_beneath 0
  in
  if err <> 0 then failwith "landlock_add_rule"

let with_fd f fd =
  Fun.protect (fun () -> f fd) ~finally:(fun () -> Unix.close fd)

let with_open_dir path f =
  let fd = open_dir path in
  with_fd f fd

let with_ruleset attrs f =
  let fd = create_ruleset attrs in
  with_fd f fd

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
  with_ruleset ruleset_attr (fun ruleset_fd ->
      with_open_dir "/usr" (fun parent_fd ->
          let path_beneath : Path_beneath_attr.t =
            { allowed_access = [ Execute; Read_file; Read_dir ]; parent_fd }
          in
          add_rule ruleset_fd path_beneath);
      no_new_privs ();
      restrict_self ruleset_fd)

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
