open Landlock

let setup_landlock rules =
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
  Ruleset.enforce_rules ruleset_attr ~rules

module Let_syntax = struct
  let ( let+ ) x f =
    let open Cmdliner.Term in
    const f $ x

  let ( and+ ) tx ty =
    let pair x y = (x, y) in
    let open Cmdliner.Term in
    const pair $ tx $ ty
end

let rule allowed_access path =
  { Landlock.Path_beneath_attr.parent = path; allowed_access }

let ro_rule = rule [ Read_file; Read_dir ]
let rx_rule = rule [ Read_file; Execute ]

let term =
  let open Let_syntax in
  let+ ro = Cmdliner.Arg.(value & opt_all string [] & info [ "ro" ])
  and+ rx = Cmdliner.Arg.(value & opt_all string [] & info [ "rx" ])
  and+ exec =
    Cmdliner.Arg.value
      (Cmdliner.Arg.pos_all Cmdliner.Arg.string [] (Cmdliner.Arg.info []))
  in
  let rules = List.map ro_rule ro @ List.map rx_rule rx in
  setup_landlock rules;
  match exec with
  | [] -> ()
  | prog :: _ as argv ->
      let fd =
        Unix.create_process prog (Array.of_list argv) Unix.stdin Unix.stdout
          Unix.stderr
      in
      let _ = Unix.waitpid [] fd in
      ()

let info = Cmdliner.Cmd.info "landlock"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
