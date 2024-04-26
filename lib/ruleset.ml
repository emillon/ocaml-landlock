open Util

let get_abi () =
  let r, _errno =
    C.Functions.Landlock.create_ruleset C.Types.Syscall.create_ruleset
      Ctypes.null Unsigned.Size_t.zero C.Types.landlock_create_ruleset_version
  in
  r

module Attr = struct
  type t = { handled_fs : Access_fs.t list; handled_net : Access_net.t list }

  let filter r ~abi =
    {
      handled_fs = List.filter (Access_fs.supported_in ~abi) r.handled_fs;
      handled_net = List.filter (Access_net.supported_in ~abi) r.handled_net;
    }

  let as_ptr { handled_fs; handled_net } =
    let open Ctypes in
    let module C = C.Types.Ruleset_attr in
    let p = allocate_n C.typ ~count:1 in
    p |-> C.handled_access_fs <-@ list_to_int Access_fs.to_int handled_fs;
    p |-> C.handled_access_net <-@ list_to_int Access_net.to_int handled_net;
    p
end

module Expert = struct
  type t = Unix.file_descr

  let create_ruleset ruleset_attr =
    let p_ruleset_attr = ruleset_attr |> Attr.as_ptr |> Ctypes.to_voidp in
    let fd, errno =
      C.Functions.Landlock.create_ruleset C.Types.Syscall.create_ruleset
        p_ruleset_attr
        (Unsigned.Size_t.of_int (Ctypes.sizeof C.Types.Ruleset_attr.typ))
        Unsigned.UInt32.zero
    in
    if fd < 0 then
      Printf.ksprintf failwith "landlock_create_ruleset: %d, errno=%s" fd
        (Signed.SInt.to_string errno);
    int_to_fd fd

  let with_ruleset attrs f =
    let fd = create_ruleset attrs in
    with_fd f fd

  let path_beneath_attr_as_ptr { Path_beneath_attr.allowed_access; parent } =
    let open Ctypes in
    let module C = C.Types.Path_beneath_attr in
    let p = allocate_n C.typ ~count:1 in
    p |-> C.allowed_access <-@ list_to_int Access_fs.to_int allowed_access;
    p |-> C.parent_fd <-@ fd_to_int parent;
    p

  let add_rule ruleset_fd path_beneath =
    let p_path_beneath =
      path_beneath_attr_as_ptr path_beneath |> Ctypes.to_voidp
    in
    let err, errno =
      C.Functions.Landlock.add_rule C.Types.Syscall.add_rule
        (fd_to_int ruleset_fd) C.Types.landlock_rule_path_beneath p_path_beneath
        0
    in
    if err <> 0 then
      Printf.ksprintf failwith "landlock_add_rule: %d, errno=%s" err
        (Signed.SInt.to_string errno)

  let restrict_self ruleset_fd =
    let err, _errno =
      C.Functions.Landlock.restrict_self C.Types.Syscall.restrict_self
        (fd_to_int ruleset_fd) 0
    in
    if err <> 0 then failwith "landlock_restrict_self"

  let no_new_privs () =
    let err, _errno = C.Functions.prctl C.Types.pr_set_no_new_privs 1 0 0 0 in
    if err <> 0 then failwith "prctl"
end

let enforce_rules attrs ~rules =
  Expert.with_ruleset attrs (fun ruleset ->
      List.iter
        (fun rule ->
          Path_beneath_attr.with_opened rule (fun fdrule ->
              Expert.add_rule ruleset fdrule))
        rules;
      Expert.no_new_privs ();
      Expert.restrict_self ruleset)
