module Operation = struct
  type t = Build | Remove | Install

  let of_string = function
    | "build" -> Build
    | "remove" -> Remove
    | "install" -> Install
    | s -> Printf.ksprintf invalid_arg "unknown operation: %s" s
end

let term =
  let open Let_syntax in
  let+ operation_s =
    Cmdliner.Arg.required
      (Cmdliner.Arg.opt
         (Cmdliner.Arg.some Cmdliner.Arg.string)
         None
         (Cmdliner.Arg.info [ "operation" ]))
  and+ cmdline =
    Cmdliner.Arg.value
      (Cmdliner.Arg.pos_all Cmdliner.Arg.string [] (Cmdliner.Arg.info []))
  in
  let operation = Operation.of_string operation_s in
  let rules =
    let rule parent allowed_access =
      { Landlock.Path_beneath_attr.parent; allowed_access }
    in
    let rx = Landlock.Access_fs.[ Read_file; Execute; Read_dir ] in
    let write =
      Landlock.Access_fs.
        [
          Make_dir;
          Make_reg;
          Write_file;
          Truncate;
          Remove_dir;
          Remove_file;
          Make_sym;
          Refer;
        ]
    in
    let rx_usr = rule "/usr" rx in
    let cwd = Sys.getcwd () in
    let opam_switch_prefix r =
      match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
      | Some dir -> [ rule dir r ]
      | None -> []
    in
    let tmp = Filename.get_temp_dir_name () in
    let read = Landlock.Access_fs.[ Read_dir; Read_file ] in
    match operation with
    | Build ->
        opam_switch_prefix rx
        @ [
            rx_usr;
            rule cwd write;
            rule tmp write;
            rule tmp read;
            rule "/dev/null" [ Read_file; Write_file ];
          ]
    | Remove -> [ rx_usr ]
    | Install ->
        opam_switch_prefix rx @ opam_switch_prefix write
        @ [ rule cwd rx; rx_usr; rule tmp write; rule tmp read ]
  in
  let abi = Landlock.Ruleset.get_abi () in
  let attrs : Landlock.Ruleset.Attr.t =
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
    |> Landlock.Ruleset.Attr.filter ~abi
  in
  Landlock.Ruleset.enforce_rules attrs ~rules;
  match cmdline with
  | [] -> ()
  | prog :: other_args -> Landlock.Util.exec prog other_args

let info = Cmdliner.Cmd.info "landlock-opam-sandbox"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
