  $ run_ppx missing_annot.ml
  File "missing_annot.ml", line 1, characters 20-21:
  1 | let%rpc f (x : int) y = ()
                          ^
  Error: Missing parameter type anotation
  [1]
