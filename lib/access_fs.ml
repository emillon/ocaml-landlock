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
  let open C.Types.Access_fs in
  function
  | Execute -> execute
  | Write_file -> write_file
  | Read_file -> read_file
  | Read_dir -> read_dir
  | Remove_dir -> remove_dir
  | Remove_file -> remove_file
  | Make_char -> make_char
  | Make_dir -> make_dir
  | Make_reg -> make_reg
  | Make_sock -> make_sock
  | Make_fifo -> make_fifo
  | Make_block -> make_block
  | Make_sym -> make_sym
  | Refer -> refer
  | Truncate -> truncate

let supported_in ~abi = function
  | Refer -> abi >= 2
  | Truncate -> abi >= 3
  | Execute | Write_file | Read_file | Read_dir | Remove_dir | Remove_file
  | Make_char | Make_dir | Make_reg | Make_sock | Make_fifo | Make_block
  | Make_sym ->
      true
