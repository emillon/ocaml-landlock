module Types (X : Ctypes.TYPE) = struct
  type ruleset_attr

  let ruleset_attr : ruleset_attr Ctypes.structure X.typ =
    X.structure "landlock_ruleset_attr"

  let handled_access_fs = X.field ruleset_attr "handled_access_fs" X.int
  let handled_access_net = X.field ruleset_attr "handled_access_net" X.int
  let () = X.seal ruleset_attr
  let landlock_access_fs_execute = X.constant "LANDLOCK_ACCESS_FS_EXECUTE" X.int

  let landlock_access_fs_write_file =
    X.constant "LANDLOCK_ACCESS_FS_WRITE_FILE" X.int

  let landlock_access_fs_read_file =
    X.constant "LANDLOCK_ACCESS_FS_READ_FILE" X.int

  let landlock_access_fs_read_dir =
    X.constant "LANDLOCK_ACCESS_FS_READ_DIR" X.int

  let landlock_access_fs_remove_dir =
    X.constant "LANDLOCK_ACCESS_FS_REMOVE_DIR" X.int

  let landlock_access_fs_remove_file =
    X.constant "LANDLOCK_ACCESS_FS_REMOVE_FILE" X.int

  let landlock_access_fs_make_char =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_CHAR" X.int

  let landlock_access_fs_make_dir =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_DIR" X.int

  let landlock_access_fs_make_reg =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_REG" X.int

  let landlock_access_fs_make_sock =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_SOCK" X.int

  let landlock_access_fs_make_fifo =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_FIFO" X.int

  let landlock_access_fs_make_block =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_BLOCK" X.int

  let landlock_access_fs_make_sym =
    X.constant "LANDLOCK_ACCESS_FS_MAKE_SYM" X.int

  let landlock_access_fs_refer = X.constant "LANDLOCK_ACCESS_FS_REFER" X.int

  let landlock_access_fs_truncate =
    X.constant "LANDLOCK_ACCESS_FS_TRUNCATE" X.int

  let landlock_access_net_bind_tcp =
    X.constant "LANDLOCK_ACCESS_NET_BIND_TCP" X.int

  let landlock_access_net_connect_tcp =
    X.constant "LANDLOCK_ACCESS_NET_CONNECT_TCP" X.int

  let sys_landlock_create_ruleset =
    X.constant "SYS_landlock_create_ruleset" X.int

  let sys_landlock_restrict_self = X.constant "SYS_landlock_restrict_self" X.int

  let landlock_create_ruleset_version =
    X.constant "LANDLOCK_CREATE_RULESET_VERSION" X.uint32_t
end

module Functions (X : Ctypes.FOREIGN) = struct
  let landlock_create_ruleset =
    X.foreign "syscall"
      Ctypes.(X.(int @-> ptr void @-> size_t @-> uint32_t @-> returning int))

  let landlock_restrict_self =
    X.foreign "syscall" Ctypes.(X.(int @-> int @-> int @-> returning int))
end
