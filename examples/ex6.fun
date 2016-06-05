let fact =
  fix \f n -> case n of
      0 -> 1
    | n -> (n * f (n - 1))
in
  fact 5
