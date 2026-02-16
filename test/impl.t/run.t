  $ run_ppx impl.ml
  include
    struct
      [%%server let f (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"impl.f" ([%json : int])
                (Os_session.connected_wrapper (fun x -> f x)))) x[@@ocaml.warning
                                                                   "-16"]]
      [%%server let f x = f x[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let f (x : int) (y : int) (z : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x y z =
          (~%
             (Eliom_client.server_function ~name:"impl.f"
                ([%json : (int * int * int)])
                (Os_session.connected_wrapper (fun (x, y, z) -> f x y z))))
            (x, y, z)[@@ocaml.warning "-16"]]
      [%%server let f x y z = f x y z[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let f (myid : Os_types.User.id) (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"impl.f" ([%json : int])
                (Os_session.connected_rpc (fun myid -> fun x -> f myid x)))) x
          [@@ocaml.warning "-16"]]
      [%%server
        let f x = f (Os_current_user.get_current_userid ()) x[@@ocaml.warning
                                                               "-16-32"]]
    end
  include
    struct
      [%%server let f (myid_o : Os_types.User.id option) (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"impl.f" ([%json : int])
                (Os_session.Opt.connected_rpc
                   (fun myid_o -> fun x -> f myid_o x)))) x[@@ocaml.warning
                                                             "-16"]]
      [%%server
        let f x = f (Os_current_user.Opt.get_current_userid ()) x[@@ocaml.warning
                                                                   "-16-32"]]
    end
  include
    struct
      [%%server let f (x : int) ~y:(y : int)  ?z:(z : int option)  () = ()]
      [%%server let _ = ()]
      [%%client
        let f x ~y  ?z  () =
          (~%
             (Eliom_client.server_function ~name:"impl.f"
                ([%json : (int * int * int option)])
                (Os_session.connected_wrapper (fun (x, y, z) -> f x ~y ?z ()))))
            (x, y, z)[@@ocaml.warning "-16"]]
      [%%server let f x ~y  ?z  () = f x ~y ?z ()[@@ocaml.warning "-16-32"]]
    end
