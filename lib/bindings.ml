module Types (X : Ctypes.TYPE) = struct
  module Ruleset_attr = struct
    type t

    let typ : t Ctypes.structure X.typ = X.structure "landlock_ruleset_attr"
    let handled_access_fs = X.field typ "handled_access_fs" X.int
    let handled_access_net = X.field typ "handled_access_net" X.int
    let () = X.seal typ
  end

  module Path_beneath_attr = struct
    type t

    let typ : t Ctypes.structure X.typ =
      X.structure "landlock_path_beneath_attr"

    let allowed_access = X.field typ "allowed_access" X.int
    let parent_fd = X.field typ "parent_fd" X.int
    let () = X.seal typ
  end

  module Access_fs = struct
    let execute = X.constant "LANDLOCK_ACCESS_FS_EXECUTE" X.int
    let write_file = X.constant "LANDLOCK_ACCESS_FS_WRITE_FILE" X.int
    let read_file = X.constant "LANDLOCK_ACCESS_FS_READ_FILE" X.int
    let read_dir = X.constant "LANDLOCK_ACCESS_FS_READ_DIR" X.int
    let remove_dir = X.constant "LANDLOCK_ACCESS_FS_REMOVE_DIR" X.int
    let remove_file = X.constant "LANDLOCK_ACCESS_FS_REMOVE_FILE" X.int
    let make_char = X.constant "LANDLOCK_ACCESS_FS_MAKE_CHAR" X.int
    let make_dir = X.constant "LANDLOCK_ACCESS_FS_MAKE_DIR" X.int
    let make_reg = X.constant "LANDLOCK_ACCESS_FS_MAKE_REG" X.int
    let make_sock = X.constant "LANDLOCK_ACCESS_FS_MAKE_SOCK" X.int
    let make_fifo = X.constant "LANDLOCK_ACCESS_FS_MAKE_FIFO" X.int
    let make_block = X.constant "LANDLOCK_ACCESS_FS_MAKE_BLOCK" X.int
    let make_sym = X.constant "LANDLOCK_ACCESS_FS_MAKE_SYM" X.int
    let refer = X.constant "LANDLOCK_ACCESS_FS_REFER" X.int
    let truncate = X.constant "LANDLOCK_ACCESS_FS_TRUNCATE" X.int
  end

  module Access_net = struct
    let bind_tcp = X.constant "LANDLOCK_ACCESS_NET_BIND_TCP" X.int
    let connect_tcp = X.constant "LANDLOCK_ACCESS_NET_CONNECT_TCP" X.int
  end

  module Syscall = struct
    let create_ruleset = X.constant "SYS_landlock_create_ruleset" X.int
    let restrict_self = X.constant "SYS_landlock_restrict_self" X.int
    let add_rule = X.constant "SYS_landlock_add_rule" X.int
  end

  let landlock_create_ruleset_version =
    X.constant "LANDLOCK_CREATE_RULESET_VERSION" X.uint32_t

  let landlock_rule_path_beneath = X.constant "LANDLOCK_RULE_PATH_BENEATH" X.int
  let pr_set_no_new_privs = X.constant "PR_SET_NO_NEW_PRIVS" X.int
  let o_path = X.constant "O_PATH" X.int
  let o_cloexec = X.constant "O_CLOEXEC" X.int
end

module Functions (X : Ctypes.FOREIGN) = struct
  module Landlock = struct
    let create_ruleset =
      X.foreign "syscall"
        Ctypes.(X.(int @-> ptr void @-> size_t @-> uint32_t @-> returning int))

    let restrict_self =
      X.foreign "syscall" Ctypes.(X.(int @-> int @-> int @-> returning int))

    let add_rule =
      X.foreign "syscall"
        Ctypes.(X.(int @-> int @-> int @-> ptr void @-> int @-> returning int))
  end

  let prctl =
    X.foreign "prctl"
      Ctypes.(X.(int @-> int @-> int @-> int @-> int @-> returning int))

  let open_ = X.foreign "open" Ctypes.(X.(string @-> int @-> returning int))
end
