module Parsetree = Ppxlib.Parsetree
module Asttypes = Ppxlib.Asttypes
module Longident = Ppxlib.Longident
module Location = Ppxlib.Location
open Ppxlib.Ast
open Ppxlib.Ast_helper

let mkloc txt loc = {txt; loc}
let mkloc_opt ?(loc = !default_loc) x = mkloc x loc
let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const.string s)
let pvar ?loc name = Pat.var ?loc (mkloc_opt ?loc name)
let ident x = Exp.ident (mkloc_opt (Longident.Lident x))

let unit ?loc ?attrs () =
  Exp.construct ?loc ?attrs (mkloc_opt ?loc (Longident.Lident "()")) None

let tunit ?loc () = Typ.constr (mkloc_opt ?loc (Longident.Lident "unit")) []

type error =
  | No_parameter
  | Missing_parameter_type
  | Missing_parameter_name
  | Reserved_parameter of string
  | Duplicated_parameter of string
  | No_return_type

let print_error ~loc (e : error) =
  let error_str =
    match e with
    | No_parameter -> "The function must have at least one parameter"
    | Missing_parameter_type -> "Missing parameter type anotation"
    | Missing_parameter_name -> "The parameter should be a variable"
    | Reserved_parameter nm ->
        Printf.sprintf "Parameter '%s' has a reserved name" nm
    | Duplicated_parameter nm ->
        Printf.sprintf "Two parameters have name '%s'" nm
    | No_return_type -> "An Lwt.t return type is mandatory"
  in
  Location.raise_errorf ~loc "%s" error_str

let rpc_name fun_name =
  let filename =
    Filename.(!Ocaml_common.Location.input_name |> chop_extension |> basename)
  in
  Format.sprintf "%s.%s" filename fun_name

let expr_tuple l =
  match l with
  | [] -> unit ()
  | [(_, x, _)] -> ident x
  | _ -> Exp.tuple (List.map (fun (_, x, _) -> ident x) l)

let pat_tuple l =
  match l with
  | [] -> Pat.any ()
  | [(_, x, _)] -> pvar x
  | _ -> Pat.tuple (List.map (fun (_, x, _) -> pvar x) l)

let typ_tuple l =
  match l with
  | [] -> tunit ()
  | [(_, _, ty)] -> ty
  | _ -> Typ.tuple (List.map (fun (_, _, ty) -> ty) l)

let expr_type e =
  match e with [%expr ([%e? _] : [%t? ty] Lwt.t)] -> Some ty | _ -> None

let process_param label def pat =
  match pat.ppat_desc with
  | Ppat_constraint ({ppat_desc = Ppat_var {txt = name}}, ty) ->
      let name = match label with Labelled n | Optional n -> n | Nolabel -> name in
      let ty =
        match label, def with
        | Optional _, Some _ ->
            let loc = ty.ptyp_loc in
            [%type: [%t ty] option]
        | _ -> ty
      in
      `Param (label, name, ty)
  | Ppat_constraint (_, ty) -> (
    match label with
    | Labelled name | Optional name ->
        let ty =
          match label, def with
          | Optional _, Some _ ->
              let loc = ty.ptyp_loc in
              [%type: [%t ty] option]
          | _ -> ty
        in
        `Param (label, name, ty)
    | Nolabel -> print_error ~loc:pat.ppat_loc Missing_parameter_name)
  | Ppat_var {txt = name} -> (
    match def with
    | Some {pexp_desc = Pexp_constraint (_, ty)} ->
        let ty =
          match label with
          | Optional _ ->
              let loc = ty.ptyp_loc in
              [%type: [%t ty] option]
          | _ -> ty
        in
        `Param (label, name, ty)
    | _ -> print_error ~loc:pat.ppat_loc Missing_parameter_type)
  | _ -> (
    match def with
    | Some {pexp_desc = Pexp_constraint (_, ty)} -> (
      match label with
      | Labelled name | Optional name ->
          let ty =
            match label with
            | Optional _ ->
                let loc = ty.ptyp_loc in
                [%type: [%t ty] option]
            | _ -> ty
          in
          `Param (label, name, ty)
      | Nolabel -> print_error ~loc:pat.ppat_loc Missing_parameter_name)
    | _ -> print_error ~loc:pat.ppat_loc Missing_parameter_type)

[%%if ocaml_version < (5, 3, 0)]

let rec collect_params l expr =
  match expr.pexp_desc with
  | Pexp_fun (label, def, pat, expr') -> (
    match pat with
    | [%pat? ()] -> (List.rev l, true), expr_type expr'
    | _ -> (
      match process_param label def pat with
      | `Param (label, name, ty) ->
          collect_params ((label, name, ty) :: l) expr'))
  | _ -> (List.rev l, false), expr_type expr

[%%else]

let rec collect_params l expr =
  match expr.pexp_desc with
  | Pexp_function (params, constraint_, Pfunction_body expr') ->
      let rec loop l params =
        match params with
        | [] ->
            let (l, has_unit), typ = collect_params l expr' in
            let typ =
              if typ <> None then typ
              else match constraint_ with
                | Some (Pconstraint {ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Lwt", "t"); _}, [ty']); _}) -> Some ty'
                | _ -> None
            in
            (l, has_unit), typ
        | param :: rest -> (
          match param.pparam_desc with
          | Pparam_val (label, def, pat) -> (
            match pat.ppat_desc with
            | Ppat_construct ({txt = Lident "()"; _}, None)
              when label = Nolabel && def = None ->
                let (l, _), typ = collect_params l expr' in
                (l, true), typ
            | _ -> (
              match process_param label def pat with
              | `Param (label, name, ty) -> loop ((label, name, ty) :: l) rest))
          | Pparam_newtype _ -> loop l rest)
      in
      loop l params
  | Pexp_constraint (e, _) ->
      let (l, has_unit), typ = collect_params l e in
      let typ = if typ = None then expr_type expr else typ in
      (l, has_unit), typ
  | _ -> (List.rev l, false), expr_type expr

[%%endif]

let parametrize loc (params, has_unit) expr =
  List.fold_right
    (fun (label, x, _) expr -> Exp.fun_ label None (pvar x) expr)
    params
    (if has_unit then [%expr fun () -> [%e expr]] else expr)

let build_params loc (params, has_unit) =
  List.map (fun (label, x, _) -> label, ident x) params
  @ if has_unit then [Nolabel, [%expr ()]] else []

let apply args expr = Exp.apply expr args

let server_function ~loc ~kind ~fun_var expr' =
  let expr =
    match kind with
    | `Connected -> [%expr fun (myid : Os_types.User.id) -> [%e expr']]
    | `Any -> [%expr fun (myid_o : Os_types.User.id option) -> [%e expr']]
    | `None -> expr'
  in
  [%stri let%server [%p fun_var] = [%e expr]]

let server_cacher ~loc ~kind ~cache ~fun_name ~fun_var ~params =
  match cache with
  | None -> [%stri let%server _ = ()]
  | Some return_typ ->
      let id_param =
        match kind with
        | `Connected -> [Nolabel, [%expr myid]]
        | `Any -> [Nolabel, [%expr myid_o]]
        | `None -> []
      in
      let cache expr =
        [%expr
          let%lwt x = [%e expr] in
          Bs_proxy.cache [%derive.caching: [%t return_typ]] x]
      in
      let parametrize_id expr =
        match kind with
        | `Connected -> [%expr fun myid -> [%e expr]]
        | `Any -> [%expr fun myid_o -> [%e expr]]
        | `None -> expr
      in
      let expr =
        fun_name |> ident
        |> apply (id_param @ build_params loc params)
        |> cache |> parametrize loc params |> parametrize_id
      in
      [%stri let%server [%p fun_var] = [%e expr] [@@ocaml.warning "-16"]]

let server_wrapper ~loc ~kind ~raw ~cache ~fun_name ~fun_var ~params =
  if raw
  then [%stri let%server _ = ()]
  else
    let id_param =
      match kind with
      | `Connected -> [Nolabel, [%expr Os_current_user.get_current_userid ()]]
      | `Any -> [Nolabel, [%expr Os_current_user.Opt.get_current_userid ()]]
      | `None -> []
    in
    let uncache expr =
      if cache <> None then [%expr Bs_proxy.extract [%e expr]] else expr
    in
    let expr =
      fun_name |> ident
      |> apply (id_param @ build_params loc params)
      |> uncache |> parametrize loc params
    in
    [%stri let%server [%p fun_var] = [%e expr] [@@ocaml.warning "-16-32"]]

let client_wrapper ~loc ~kind ~raw ~cache ~fun_name ~fun_var ~params =
  let id_param =
    match kind with
    | `Connected -> [Nolabel, [%expr myid]]
    | `Any -> [Nolabel, [%expr myid_o]]
    | `None -> []
  in
  let uncache expr =
    if cache <> None then [%expr Bs_proxy.extract [%e expr]] else expr
  in
  let parametrize' expr =
    [%expr fun [%p pat_tuple (fst params)] -> [%e expr]]
  in
  let parametrize_id expr =
    match kind with
    | `Connected -> [%expr fun myid -> [%e expr]]
    | `Any -> [%expr fun myid_o -> [%e expr]]
    | `None -> expr
  in
  let wrap expr =
    if raw
    then expr
    else
      match kind with
      | `Connected -> [%expr Os_session.connected_rpc [%e expr]]
      | `Any -> [%expr Os_session.Opt.connected_rpc [%e expr]]
      | `None -> [%expr Os_session.connected_wrapper [%e expr]]
  in
  let expr =
    fun_name |> ident
    |> apply (id_param @ build_params loc params)
    |> uncache |> parametrize' |> parametrize_id |> wrap
  in
  let expr =
    [%expr
      ~%(Eliom_client.server_function
           ~name:[%e str (rpc_name fun_name)]
           [%json: [%t typ_tuple (fst params)]] [%e expr])
        [%e expr_tuple (fst params)]]
  in
  [%stri
  let%client [%p fun_var] = [%e parametrize loc params expr]
  [@@ocaml.warning "-16"]]

let raw = ref false
let cache = ref false

[%%if ocaml_version < (5, 3, 0)]

let rec check_myid expr =
  match expr with
  | [%expr fun myid -> [%e? expr']] -> `Connected, expr'
  | [%expr fun myid_o -> [%e? expr']] -> `Any, expr'
  | {pexp_desc = Pexp_constraint (e, t)} ->
      let kind, new_e = check_myid e in
      if kind <> `None
      then kind, {expr with pexp_desc = Pexp_constraint (new_e, t)}
      else `None, expr
  | _ -> `None, expr

[%%else]

let rec check_myid expr =
  match expr with
  | [%expr fun myid -> [%e? expr']] -> `Connected, expr'
  | [%expr fun myid_o -> [%e? expr']] -> `Any, expr'
  | {pexp_desc = Pexp_function (params, constraint_, Pfunction_body e)} -> (
    match params with
    | { pparam_desc =
          Pparam_val (Nolabel, None, {ppat_desc = Ppat_var {txt = "myid"}}) }
      :: rest ->
        let expr' = match rest with
          | [] -> e
          | _ -> {expr with pexp_desc = Pexp_function (rest, constraint_, Pfunction_body e)}
        in
        `Connected, expr'
    | { pparam_desc =
          Pparam_val (Nolabel, None, {ppat_desc = Ppat_var {txt = "myid_o"}})
      }
      :: rest ->
        let expr' = match rest with
          | [] -> e
          | _ -> {expr with pexp_desc = Pexp_function (rest, constraint_, Pfunction_body e)}
        in
        `Any, expr'
    | _ -> `None, expr)
  | {pexp_desc = Pexp_constraint (e, t)} ->
      let kind, new_e = check_myid e in
      if kind <> `None
      then kind, {expr with pexp_desc = Pexp_constraint (new_e, t)}
      else `None, expr
  | _ -> `None, expr

[%%endif]

let extension_impl ~legacy ~loc ~path:_ ~return_typ_hint fun_name expr =
  let raw = !raw && not !cache in
  let cache = (not legacy) && !cache in
  let fun_var = pvar ~loc:fun_name.loc fun_name.txt in
  let fun_name = fun_name.txt in
  let kind, expr' = if raw then `None, expr else check_myid expr in
  let params, return_typ = collect_params [] expr' in
  let return_typ = match return_typ with Some _ -> return_typ | None -> return_typ_hint in
  (match params with
  | [], false -> print_error ~loc No_parameter
  | l, _ ->
      ignore
        (List.fold_left
           (fun acc (_, nm, _) ->
              if List.mem nm acc then print_error ~loc (Duplicated_parameter nm);
              if nm = "myid" || nm = "myid_o"
              then print_error ~loc (Reserved_parameter nm);
              nm :: acc)
           [] l));
  if cache && return_typ = None then print_error ~loc No_return_type;
  let cache = if cache then return_typ else None in
  Str.include_ ~loc
    (Incl.mk ~loc
       (Mod.structure ~loc
          [ server_function ~loc ~kind ~fun_var expr'
          ; server_cacher ~loc ~kind ~cache ~fun_name ~fun_var ~params
          ; client_wrapper ~loc ~kind ~raw ~cache ~fun_name ~fun_var ~params
          ; server_wrapper ~loc ~kind ~raw ~cache ~fun_name ~fun_var ~params ]))

[%%if ocaml_version < (5, 3, 0)]

let extension ~legacy ~loc ~path fun_name expr =
  extension_impl ~legacy ~loc ~path ~return_typ_hint:None fun_name expr

let vb_pattern =
  let open Ppxlib.Ast_pattern in
  value_binding ~pat:(ppat_var __') ~expr:__

[%%else]

let rec return_type_of_arrow ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (_, _, ret) -> return_type_of_arrow ret
  | _ -> ty

let lwt_return_type constraint_opt =
  match constraint_opt with
  | Some (Pvc_constraint {typ; _}) -> (
      let ret = return_type_of_arrow typ in
      match ret.ptyp_desc with
      | Ptyp_constr ({txt = Ldot (Lident "Lwt", "t"); _}, [ty']) -> Some ty'
      | _ -> None)
  | _ -> None

let extension ~legacy ~loc ~path fun_name expr constraint_opt =
  extension_impl ~legacy ~loc ~path
    ~return_typ_hint:(lwt_return_type constraint_opt) fun_name expr

let vb_pattern =
  let open Ppxlib.Ast_pattern in
  value_binding ~pat:(ppat_var __') ~expr:__ ~constraint_:__

[%%endif]

let pattern =
  let open Ppxlib.Ast_pattern in
  pstr (pstr_value nonrecursive (vb_pattern ^:: nil) ^:: nil)

let extensions =
  let open Ppxlib in
  List.concat
  @@ List.map
       (fun (legacy, exts) ->
          List.map
            (fun ext ->
               Extension.declare ext Extension.Context.structure_item pattern
                 (extension ~legacy))
            exts)
       [true, ["cw_rpc"; "crpc"; "crpc_opt"]; false, ["rpc"]]

let driver_args =
  [ ( "--rpc-raw"
    , Arg.Unit (fun () -> raw := true)
    , " Do not insert any ocsigen-start session wrapper." )
  ; ( "--rpc-cache"
    , Arg.Unit (fun () -> cache := true)
    , " Insert caching directives (for internal use at Be Sport)." ) ]

let () =
  List.iter
    (fun (key, spec, doc) -> Ppxlib.Driver.add_arg key spec ~doc)
    driver_args

let rules = List.map Ppxlib.Context_free.Rule.extension extensions
let () = Ppxlib.Driver.register_transformation ~rules "rpc"
