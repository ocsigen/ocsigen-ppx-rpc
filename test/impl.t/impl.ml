let%rpc f (x : int) = ()
let%rpc f (x : int) (y : int) (z : int) = ()

let%rpc f myid (x : int) = ()
let%rpc f myid_o (x : int) = ()

let%rpc f (x : int) ~(y : int) ?(z : int option) () = ()
