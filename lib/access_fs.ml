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
