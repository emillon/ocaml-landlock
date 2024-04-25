module Attr : sig
  type t = { handled_fs : Access_fs.t list; handled_net : Access_net.t list }

  val filter : t -> abi:int -> t
end

val get_abi : unit -> int
val enforce_rules : Attr.t -> rules:string Path_beneath_attr.t list -> unit

module Expert : sig
  type t

  val with_ruleset : Attr.t -> (t -> 'a) -> 'a
  val add_rule : t -> Unix.file_descr Path_beneath_attr.t -> unit
  val no_new_privs : unit -> unit
  val restrict_self : t -> unit
end
