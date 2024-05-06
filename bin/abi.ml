let term =
  let open Let_syntax in
  let+ () = Cmdliner.Term.const () in
  let abi = Landlock.Ruleset.get_abi () in
  Printf.printf "ABI: %d\n" abi

let info = Cmdliner.Cmd.info "abi"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
