open Cmdliner.Term

let pair x y = (x, y)
let ( let+ ) x f = const f $ x
let ( and+ ) tx ty = const pair $ tx $ ty
