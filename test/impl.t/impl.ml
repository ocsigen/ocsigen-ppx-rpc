let%rpc f (x : int) = ()
let%rpc f (x : int) (y : int) (z : int) = ()

let%rpc f (x : int) : unit = ()
let%rpc f = fun (x : int) : unit -> ()
