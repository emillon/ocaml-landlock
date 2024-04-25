type t = Bind_tcp | Connect_tcp

let to_int =
  let open C.Types in
  function
  | Bind_tcp -> landlock_access_net_bind_tcp
  | Connect_tcp -> landlock_access_net_connect_tcp

let supported_in ~abi = function Bind_tcp | Connect_tcp -> abi >= 4
