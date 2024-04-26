type t = Bind_tcp | Connect_tcp

let to_int =
  let open C.Types.Access_net in
  function Bind_tcp -> bind_tcp | Connect_tcp -> connect_tcp

let supported_in ~abi = function Bind_tcp | Connect_tcp -> abi >= 4
