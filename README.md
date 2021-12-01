ocsigen-ppx-rpc
===============

This PPX adds a syntax for RPCs for Eliom and Ocsigen Start.


Installation
------------

_ocsigen-ppx-rpc_ can be installed via [OPAM](https://opam.ocaml.org):

    opam install ppx_deriving

Buildsystem integration
-----------------------

With Dune, you should add a `preprocess` directive to your target:

    (executable
      (libraries whatever)
      (preprocess (pps ocsigen-ppx-rpc))
      (name blah))

Usage
-----

This PPX let you defined RPCs:
```ocaml
let%rpc f (x1 : t1) ... (xn : tn) = e
```
These functions are defined server-side and can be used both on the
client and on the server.

An additional initial parameter can be used to refer to the current
user following Ocsigen Start's convention:
```ocaml
let%rpc f myid (x1 : t1) ... (xn : tn) = e
let%rpc f myid_o (x1 : t1) ... (xn : tn) = e
```
It is automatically bound: it does not have to provided when invoking
the function

Parameters can also be named ~(x : t) or optional ?(x : t option).  A
unit parameter is allowed at the very end, which can be convenient
when using optional parameters:
```ocaml
let%rpc f ?(x1 : t1) ... ~(xn : tn) () = e
```

By default, this PPX is meant to be used together with Ocsigen
Start. So, it automatically inserts `Os_session` wrappers. If you want
to use it with Eliom but without Ocsigen Start, you can use the option
`--rpc-raw`

    (executable
      (libraries whatever)
      (preprocess (pps ocsigen-ppx-rpc --rpc-raw))
      (name blah))
