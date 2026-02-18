  $ run_ppx missing_annot.ml
  File "missing_annot.ml", line 1, characters 20-21:
  1 | let%rpc f (x : int) y = ()
                          ^
  Error: Missing parameter type anotation
  [1]
  $ run_ppx no_param.ml
  File "no_param.ml", line 1, characters 0-14:
  1 | let%rpc f = ()
      ^^^^^^^^^^^^^^
  Error: The function must have at least one parameter
  [1]
  $ run_ppx no_param_myid.ml
  File "no_param_myid.ml", line 1, characters 0-19:
  1 | let%rpc f myid = ()
      ^^^^^^^^^^^^^^^^^^^
  Error: The function must have at least one parameter
  [1]
