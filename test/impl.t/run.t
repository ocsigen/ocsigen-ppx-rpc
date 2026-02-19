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
      [%%server let f (x : int) : unit= ()]
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
      [%%server let f (x : int) : unit= ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"impl.f" ([%json : int])
                (Os_session.connected_wrapper (fun x -> f x)))) x[@@ocaml.warning
                                                                   "-16"]]
      [%%server let f x = f x[@@ocaml.warning "-16-32"]]
    end

  $ run_ppx labels.ml
  include
    struct
      [%%server let nolabel (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let nolabel x =
          (~%
             (Eliom_client.server_function ~name:"labels.nolabel"
                ([%json : int])
                (Os_session.connected_wrapper (fun x -> nolabel x)))) x
          [@@ocaml.warning "-16"]]
      [%%server let nolabel x = nolabel x[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let labelled ~x:(x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let labelled ~x =
          (~%
             (Eliom_client.server_function ~name:"labels.labelled"
                ([%json : int])
                (Os_session.connected_wrapper (fun x -> labelled ~x)))) x
          [@@ocaml.warning "-16"]]
      [%%server let labelled ~x = labelled ~x[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let optional ?x:(x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let optional ?x =
          (~%
             (Eliom_client.server_function ~name:"labels.optional"
                ([%json : int])
                (Os_session.connected_wrapper (fun x -> optional ?x)))) x
          [@@ocaml.warning "-16"]]
      [%%server let optional ?x = optional ?x[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let optional_with_def ?x:((x : int)= 1) = ()]
      [%%server let _ = ()]
      [%%client
        let optional_with_def ?x =
          (~%
             (Eliom_client.server_function ~name:"labels.optional_with_def"
                ([%json : int option])
                (Os_session.connected_wrapper (fun x -> optional_with_def ?x))))
            x[@@ocaml.warning "-16"]]
      [%%server
        let optional_with_def ?x = optional_with_def ?x[@@ocaml.warning
                                                         "-16-32"]]
    end
  include
    struct
      [%%server let optional_with_def2 ?(x= (1 : int)) = ()]
      [%%server let _ = ()]
      [%%client
        let optional_with_def2 ?x =
          (~%
             (Eliom_client.server_function ~name:"labels.optional_with_def2"
                ([%json : int option])
                (Os_session.connected_wrapper (fun x -> optional_with_def2 ?x))))
            x[@@ocaml.warning "-16"]]
      [%%server
        let optional_with_def2 ?x = optional_with_def2 ?x[@@ocaml.warning
                                                           "-16-32"]]
    end

  $ run_ppx myid.ml
  include
    struct
      [%%server let f (myid : Os_types.User.id) (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"myid.f" ([%json : int])
                (Os_session.connected_rpc (fun myid x -> f myid x)))) x
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
             (Eliom_client.server_function ~name:"myid.f" ([%json : int])
                (Os_session.Opt.connected_rpc (fun myid_o x -> f myid_o x)))) x
          [@@ocaml.warning "-16"]]
      [%%server
        let f x = f (Os_current_user.Opt.get_current_userid ()) x[@@ocaml.warning
                                                                   "-16-32"]]
    end
  include
    struct
      [%%server let f (myid : Os_types.User.id) () = ()]
      [%%server let _ = ()]
      [%%client
        let f () =
          (~%
             (Eliom_client.server_function ~name:"myid.f" ([%json : unit])
                (Os_session.connected_rpc (fun myid _ -> f myid ())))) ()
          [@@ocaml.warning "-16"]]
      [%%server
        let f () = f (Os_current_user.get_current_userid ()) ()[@@ocaml.warning
                                                                 "-16-32"]]
    end

  $ run_ppx unit.ml
  include
    struct
      [%%server let f (x : int) () = ()]
      [%%server let _ = ()]
      [%%client
        let f x () =
          (~%
             (Eliom_client.server_function ~name:"unit.f" ([%json : int])
                (Os_session.connected_wrapper (fun x -> f x ())))) x[@@ocaml.warning
                                                                      "-16"]]
      [%%server let f x () = f x ()[@@ocaml.warning "-16-32"]]
    end
  include
    struct
      [%%server let f () = ()]
      [%%server let _ = ()]
      [%%client
        let f () =
          (~%
             (Eliom_client.server_function ~name:"unit.f" ([%json : unit])
                (Os_session.connected_wrapper (fun _ -> f ())))) ()[@@ocaml.warning
                                                                     "-16"]]
      [%%server let f () = f ()[@@ocaml.warning "-16-32"]]
    end

  $ run_ppx newtype.ml
  include
    struct
      [%%server let f (type a) (x : int) = ()]
      [%%server let _ = ()]
      [%%client
        let f x =
          (~%
             (Eliom_client.server_function ~name:"newtype.f" ([%json : int])
                (Os_session.connected_wrapper (fun x -> f x)))) x[@@ocaml.warning
                                                                   "-16"]]
      [%%server let f x = f x[@@ocaml.warning "-16-32"]]
    end
